package chat

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.Stream
import scala.concurrent.duration.*
import java.util.Base64
import javax.sound.sampled.{AudioFormat, AudioSystem, SourceDataLine, TargetDataLine}

// Raw PCM voice over the existing TLS line protocol: 16 kHz, 16-bit signed LE,
// mono, 40 ms frames. All JDK-only (javax.sound.sampled); no new dependencies.
object Audio:
  private val sampleRate = 16000f
  private val frameSamples = 640 // 40 ms at 16 kHz
  private val frameBytes = frameSamples * 2

  // VOX: only send frames whose RMS clears the threshold, plus a short hangover
  // after speech so word tails aren't clipped. Tuned constants, no config knob.
  private val voxRmsThreshold = 450.0
  private val hangoverFrames = 6 // ~240 ms

  // Per-sender jitter buffer; drop-oldest on overflow (circularBuffer).
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
        .takeWhile(_.isDefined)
        .map(_.get)
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

    // Every 40 ms take at most one frame per active sender and write their sum
    // (clamped to Short range). One speaker passes through; several mix.
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

  def open(checkMuted: IO[Boolean]): Resource[IO, Handle] =
    for
      capture <- Resource.make(IO.blocking {
        val line = AudioSystem.getTargetDataLine(format)
        line.open(format, lineBufferBytes)
        line.start()
        line
      })(l => IO.blocking { l.stop(); l.close() })
      playback <- Resource.make(IO.blocking {
        val line = AudioSystem.getSourceDataLine(format)
        line.open(format, lineBufferBytes)
        line.start()
        line
      })(l => IO.blocking { l.flush(); l.stop(); l.close() })
      senders <- Resource.eval(Ref.of[IO, Map[String, Queue[IO, Array[Short]]]](Map.empty))
      hangover <- Resource.eval(Ref.of[IO, Int](0))
    yield new Handle(senders, hangover, capture, playback, checkMuted)

  private def readFrame(line: TargetDataLine): Option[Array[Byte]] =
    val buf = new Array[Byte](frameBytes)
    var off = 0
    var live = true
    while off < frameBytes && live do
      val n = line.read(buf, off, frameBytes - off)
      if n <= 0 then live = false else off += n
    Option.when(off == frameBytes)(buf)

  private def rms(buf: Array[Byte]): Double =
    var sum = 0.0
    var i = 0
    while i + 1 < buf.length do
      val s = (((buf(i + 1) & 0xff) << 8) | (buf(i) & 0xff)).toShort
      sum += s.toDouble * s.toDouble
      i += 2
    math.sqrt(sum / (buf.length / 2))

  private def decodeFrame(b64: String): Array[Short] =
    val bytes = Base64.getDecoder.decode(b64)
    val out = new Array[Short](bytes.length / 2)
    var i = 0
    while i < out.length do
      out(i) = (((bytes(i * 2 + 1) & 0xff) << 8) | (bytes(i * 2) & 0xff)).toShort
      i += 1
    out

  private def mix(frames: List[Array[Short]]): Array[Byte] =
    val out = new Array[Byte](frameBytes)
    var i = 0
    while i < frameSamples do
      var acc = 0
      frames.foreach(f => if i < f.length then acc += f(i))
      val clamped = math.max(Short.MinValue.toInt, math.min(Short.MaxValue.toInt, acc))
      out(i * 2) = (clamped & 0xff).toByte
      out(i * 2 + 1) = ((clamped >> 8) & 0xff).toByte
      i += 1
    out
