package chat

import cats.effect.*
import cats.effect.std.Queue
import cats.mtl.Raise
import cats.syntax.all.*
import fs2.Stream

import java.io.ByteArrayInputStream
import java.util.Base64
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.SourceDataLine
import javax.sound.sampled.TargetDataLine
import scala.concurrent.duration.*
import scala.sys.process.*

object Audio:
  final case class AudioError(message: String)

  private def orRaise[A](io: IO[A])(using r: Raise[IO, AudioError]): IO[A] =
    io.handleErrorWith(e => r.raise(AudioError(Option(e.getMessage).getOrElse(e.toString))))

  private val sampleRate = 16000f
  private val frameSamples = 640 // 40 ms at 16 kHz
  private val frameBytes = frameSamples * 2

  private val voxRmsThreshold = 450.0
  private val hangoverFrames = 6 // ~240 ms

  private val jitterCapacity = 5
  private val lineBufferBytes = frameBytes * 4

  private val format =
    new AudioFormat(sampleRate, 16, 1, /*signed*/ true, /*bigEndian*/ false)

  final class Handle(
      senders: Ref[IO, Map[String, Queue[IO, Array[Short]]]],
      hangover: Ref[IO, Int],
      capture: TargetDataLine,
      playbackLine: SourceDataLine,
      checkMuted: IO[Boolean]
  ):
    val frames: Stream[IO, String] =
      Stream
        .repeatEval(IO.blocking(readFrame(capture)))
        .unNoneTerminate
        .evalMap { buf =>
          checkMuted.flatMap { muted =>
            if muted then hangover.set(0).as(None)
            else
              val loud = rms(buf) >= voxRmsThreshold
              hangover
                .modify { h =>
                  if loud then (hangoverFrames, true)
                  else if h > 0 then (h - 1, true)
                  else (0, false)
                }
                .map(send => Option.when(send)(Base64.getEncoder.encodeToString(buf)))
          }
        }
        .unNone

    def receive(from: String, b64: String): IO[Unit] =
      IO(decodeFrame(b64)).flatMap { frame =>
        senders.get.flatMap { m =>
          m.get(from) match
            case Some(q) => q.offer(frame)
            case None =>
              Queue.circularBuffer[IO, Array[Short]](jitterCapacity).flatMap { q =>
                senders.update(_ + (from -> q)) *> q.offer(frame)
              }
        }
      }

    val playback: Stream[IO, Unit] =
      Stream.awakeEvery[IO](40.millis).evalMap { _ =>
        senders.get
          .flatMap(_.values.toList.traverse(_.tryTake))
          .map(_.flatten)
          .flatMap { taken =>
            if taken.isEmpty then IO.unit
            else
              val mixed = mix(taken)
              IO.blocking(playbackLine.write(mixed, 0, mixed.length)).void
          }
      }

  private val toneFreq = 880.0
  private val toneMillis = 120
  private val gapMillis = 60
  private val toneRate = 44100

  def playTone(critical: Boolean)(using r: Raise[IO, AudioError]): IO[Unit] =
    playViaPipeWire(toneSequence(toneRate.toFloat, 1, critical)).flatMap {
      case true  => IO.unit
      case false => playViaJavaSound(critical)
    }

  private def playViaPipeWire(pcm: Array[Byte]): IO[Boolean] =
    IO.blocking {
      val cmd = Seq(
        "pw-play",
        "--format",
        "s16",
        "--rate",
        toneRate.toString,
        "--channels",
        "1",
        "--raw",
        "-"
      )
      val quiet = ProcessLogger(_ => (), _ => ())
      (Process(cmd) #< new ByteArrayInputStream(pcm)).!(quiet) == 0
    }.handleError(_ => false)

  private val toneFormats: List[AudioFormat] = List(
    new AudioFormat(44100f, 16, 2, /*signed*/ true, /*bigEndian*/ false),
    new AudioFormat(48000f, 16, 2, /*signed*/ true, /*bigEndian*/ false),
    format
  )

  private def playViaJavaSound(critical: Boolean)(using r: Raise[IO, AudioError]): IO[Unit] =
    toneFormats
      .collectFirstSomeM(fmt => playToneWith(fmt, critical).attempt.map(_.toOption))
      .flatMap {
        case Some(_) => IO.unit
        case None    => r.raise(AudioError("no audio output available for the alert tone"))
      }

  private def playToneWith(fmt: AudioFormat, critical: Boolean): IO[Unit] =
    IO.blocking {
      val line = AudioSystem.getSourceDataLine(fmt)
      line.open(fmt)
      line.start()
      val pcm = toneSequence(fmt.getSampleRate, fmt.getChannels, critical)
      line.write(pcm, 0, pcm.length)
      line.drain()
      line.stop()
      line.close()
    }

  private def toneSequence(sr: Float, channels: Int, critical: Boolean): Array[Byte] =
    val beep = toneBytes(sr, channels, toneMillis)
    if !critical then beep
    else
      val gap = new Array[Byte]((sr * gapMillis / 1000).toInt * channels * 2)
      beep ++ gap ++ beep

  private def toneBytes(sr: Float, channels: Int, millis: Int): Array[Byte] =
    val n = (sr * millis / 1000).toInt
    val fade = (sr * 0.005).max(1)
    (0 until n).iterator.flatMap { i =>
      val env = math.min(1.0, math.min(i, n - i) / fade)
      val s =
        (math.sin(2.0 * math.Pi * toneFreq * i / sr) * env * Short.MaxValue * 0.4).toShort
      val lo = (s & 0xff).toByte
      val hi = ((s >> 8) & 0xff).toByte
      (0 until channels).iterator.flatMap(_ => Iterator(lo, hi))
    }.toArray

  def open(checkMuted: IO[Boolean])(using Raise[IO, AudioError]): Resource[IO, Handle] =
    for
      capture <- Resource.make(orRaise(IO.blocking {
        val line = AudioSystem.getTargetDataLine(format)
        line.open(format, lineBufferBytes)
        line.start()
        line
      }))(l => IO.blocking { l.stop(); l.close() })
      playback <- Resource.make(orRaise(IO.blocking {
        val line = AudioSystem.getSourceDataLine(format)
        line.open(format, lineBufferBytes)
        line.start()
        line
      }))(l => IO.blocking { l.flush(); l.stop(); l.close() })
      senders <- Resource.eval(Ref.of[IO, Map[String, Queue[IO, Array[Short]]]](Map.empty))
      hangover <- Resource.eval(Ref.of[IO, Int](0))
    yield new Handle(senders, hangover, capture, playback, checkMuted)

  private def readFrame(line: TargetDataLine): Option[Array[Byte]] =
    val buf = new Array[Byte](frameBytes)
    @annotation.tailrec
    def fill(off: Int): Int =
      if off >= frameBytes then off
      else
        val n = line.read(buf, off, frameBytes - off)
        if n <= 0 then off else fill(off + n)
    Option.when(fill(0) == frameBytes)(buf)

  private def rms(buf: Array[Byte]): Double =
    val samples = buf.length / 2
    val sum = (0 until samples).foldLeft(0.0) { (acc, i) =>
      val s = (((buf(i * 2 + 1) & 0xff) << 8) | (buf(i * 2) & 0xff)).toShort
      acc + s.toDouble * s.toDouble
    }
    math.sqrt(sum / samples)

  private def decodeFrame(b64: String): Array[Short] =
    val bytes = Base64.getDecoder.decode(b64)
    Array.tabulate(bytes.length / 2) { i =>
      (((bytes(i * 2 + 1) & 0xff) << 8) | (bytes(i * 2) & 0xff)).toShort
    }

  private def mix(frames: List[Array[Short]]): Array[Byte] =
    val out = new Array[Byte](frameBytes)
    (0 until frameSamples).foreach { i =>
      val acc = frames.foldLeft(0)((a, f) => if i < f.length then a + f(i) else a)
      val clamped = math.max(Short.MinValue.toInt, math.min(Short.MaxValue.toInt, acc))
      out(i * 2) = (clamped & 0xff).toByte
      out(i * 2 + 1) = ((clamped >> 8) & 0xff).toByte
    }
    out
