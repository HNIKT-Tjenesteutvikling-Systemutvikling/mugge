package chat

import cats.Applicative
import cats.effect.*
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.mtl.Raise
import cats.syntax.all.*
import fs2.Chunk
import fs2.Stream
import fs2.io.process.ProcessBuilder
import fs2.io.process.Processes

import java.io.ByteArrayInputStream
import java.util.Base64
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.TargetDataLine
import scala.concurrent.duration.*
import scala.sys.process.*

final case class AudioError(message: String)

object Audio:
  val sampleRate = 16000f
  val frameSamples = 640 // 40 ms at 16 kHz
  val frameBytes = frameSamples * 2

  val jitterCapacity = 5

final class AudioHandle[F[_]: Async](
    senders: Ref[F, Map[String, Queue[F, Array[Short]]]],
    hangover: Ref[F, Int],
    rawFrames: Stream[F, Array[Byte]],
    writeFrame: Array[Byte] => F[Unit],
    checkMuted: F[Boolean],
    val backend: String
):
  import AudioHandle.*

  val frames: Stream[F, String] =
    rawFrames.evalMap { buf =>
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
    }.unNone

  def receive(from: String, b64: String): F[Unit] =
    Sync[F].delay(decodeFrame(b64)).flatMap { frame =>
      senders.get.flatMap { m =>
        m.get(from) match
          case Some(q) => q.offer(frame)
          case None =>
            Queue.circularBuffer[F, Array[Short]](Audio.jitterCapacity).flatMap { q =>
              senders.update(_ + (from -> q)) *> q.offer(frame)
            }
      }
    }

  val playback: Stream[F, Unit] =
    Stream.awakeEvery[F](40.millis).evalMap { _ =>
      senders.get
        .flatMap(_.values.toList.traverse(_.tryTake))
        .map(_.flatten)
        .flatMap { taken =>
          if taken.isEmpty then Applicative[F].unit
          else writeFrame(mix(taken))
        }
    }

object AudioHandle:
  private val voxRmsThreshold = 450.0
  private val hangoverFrames = 6 // ~240 ms

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
    val out = new Array[Byte](Audio.frameBytes)
    (0 until Audio.frameSamples).foreach { i =>
      val acc = frames.foldLeft(0)((a, f) => if i < f.length then a + f(i) else a)
      val clamped = math.max(Short.MinValue.toInt, math.min(Short.MaxValue.toInt, acc))
      out(i * 2) = (clamped & 0xff).toByte
      out(i * 2 + 1) = ((clamped >> 8) & 0xff).toByte
    }
    out

trait Audio[F[_]]:
  def playTone(critical: Boolean)(using Raise[F, AudioError]): F[Unit]
  def open(checkMuted: F[Boolean])(using Raise[F, AudioError]): Resource[F, AudioHandle[F]]

final class LiveAudio[F[_]: Async: Processes] private () extends Audio[F]:
  private val lineBufferBytes = Audio.frameBytes * 4

  private val format =
    new AudioFormat(Audio.sampleRate, 16, 1, /*signed*/ true, /*bigEndian*/ false)

  private val toneFreq = 880.0
  private val toneMillis = 120
  private val gapMillis = 60
  private val toneRate = 44100

  private val toneFormats: List[AudioFormat] = List(
    new AudioFormat(44100f, 16, 2, /*signed*/ true, /*bigEndian*/ false),
    new AudioFormat(48000f, 16, 2, /*signed*/ true, /*bigEndian*/ false),
    format
  )

  private val pipewireArgs = List(
    "--format",
    "s16",
    "--rate",
    Audio.sampleRate.toInt.toString,
    "--channels",
    "1",
    "--raw",
    "--latency",
    "40ms",
    "-"
  )

  override def playTone(critical: Boolean)(using r: Raise[F, AudioError]): F[Unit] =
    playViaPipeWire(toneSequence(toneRate.toFloat, 1, critical)).flatMap {
      case true  => Applicative[F].unit
      case false => playViaJavaSound(critical)
    }

  override def open(checkMuted: F[Boolean])(using
      Raise[F, AudioError]
  ): Resource[F, AudioHandle[F]] =
    for
      io <- pipewireIo.handleErrorWith((_: Throwable) => javaSoundIo)
      senders <- Resource.eval(Ref.of[F, Map[String, Queue[F, Array[Short]]]](Map.empty))
      hangover <- Resource.eval(Ref.of[F, Int](0))
    yield new AudioHandle[F](senders, hangover, io.frames, io.write, checkMuted, io.name)

  private def orRaise[A](fa: F[A])(using r: Raise[F, AudioError]): F[A] =
    fa.handleErrorWith(e => r.raise(AudioError(Option(e.getMessage).getOrElse(e.toString))))

  private def playViaPipeWire(pcm: Array[Byte]): F[Boolean] =
    Sync[F]
      .blocking {
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
      }
      .handleError(_ => false)

  private def playViaJavaSound(critical: Boolean)(using r: Raise[F, AudioError]): F[Unit] =
    toneFormats
      .collectFirstSomeM(fmt => playToneWith(fmt, critical).attempt.map(_.toOption))
      .flatMap {
        case Some(_) => Applicative[F].unit
        case None    => r.raise(AudioError("no audio output available for the alert tone"))
      }

  private def playToneWith(fmt: AudioFormat, critical: Boolean): F[Unit] =
    Sync[F].blocking {
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

  private final case class Io(
      frames: Stream[F, Array[Byte]],
      write: Array[Byte] => F[Unit],
      name: String
  )

  private def pipewireIo: Resource[F, Io] =
    for
      recorder <- ProcessBuilder("pw-record", pipewireArgs).spawn[F]
      player <- ProcessBuilder("pw-play", pipewireArgs).spawn[F]
      outgoing <- Resource.eval(Queue.circularBuffer[F, Chunk[Byte]](Audio.jitterCapacity))
      _ <- Stream
        .fromQueueUnterminated(outgoing)
        .flatMap(Stream.chunk)
        .through(player.stdin)
        .compile
        .drain
        .background
      _ <- recorder.stderr.merge(player.stderr).compile.drain.background
    yield Io(
      recorder.stdout.chunkN(Audio.frameBytes, allowFewer = false).map(_.toArray),
      bytes => outgoing.offer(Chunk.array(bytes)),
      "pipewire"
    )

  private def javaSoundIo(using Raise[F, AudioError]): Resource[F, Io] =
    for
      capture <- Resource.make(orRaise(Sync[F].blocking {
        val line = AudioSystem.getTargetDataLine(format)
        line.open(format, lineBufferBytes)
        line.start()
        line
      }))(l => Sync[F].blocking { l.stop(); l.close() })
      playback <- Resource.make(orRaise(Sync[F].blocking {
        val line = AudioSystem.getSourceDataLine(format)
        line.open(format, lineBufferBytes)
        line.start()
        line
      }))(l => Sync[F].blocking { l.flush(); l.stop(); l.close() })
    yield Io(
      Stream.repeatEval(Sync[F].blocking(readFrame(capture))).unNoneTerminate,
      bytes => Sync[F].blocking(playback.write(bytes, 0, bytes.length)).void,
      "javasound"
    )

  private def readFrame(line: TargetDataLine): Option[Array[Byte]] =
    val buf = new Array[Byte](Audio.frameBytes)
    @annotation.tailrec
    def fill(off: Int): Int =
      if off >= Audio.frameBytes then off
      else
        val n = line.read(buf, off, Audio.frameBytes - off)
        if n <= 0 then off else fill(off + n)
    Option.when(fill(0) == Audio.frameBytes)(buf)

object LiveAudio:
  def apply[F[_]: Async: Processes](): Audio[F] = new LiveAudio[F]()
