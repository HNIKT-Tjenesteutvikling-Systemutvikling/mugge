package chat

import cats.effect.*
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.mtl.Handle.allow
import cats.syntax.all.*

final case class VoiceSession[F[_]](handle: AudioHandle[F], teardown: F[Unit])

trait Voice[F[_]]:
  def toggle(
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit]

  def toggleTest(
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit]

  def toggleMute(
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit]

final class LiveVoice[F[_]: Concurrent] private (audio: Audio[F]) extends Voice[F]:

  override def toggle(
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit] =
    voiceRef.get.flatMap {
      case Some(_) => stop(state, outgoingQueue, ui, voiceRef)
      case None    => start(state, outgoingQueue, ui, voiceRef, loopback = false)
    }

  override def toggleTest(
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit] =
    voiceRef.get.flatMap {
      case Some(_) => stop(state, outgoingQueue, ui, voiceRef)
      case None    => start(state, outgoingQueue, ui, voiceRef, loopback = true)
    }

  private def start(
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]],
      loopback: Boolean
  ): F[Unit] =
    allow[AudioError] {
      val micMuted = state.get.map(st => st.muted || st.adminMuted)
      audio.open(micMuted).allocated.flatMap { case (handle, release) =>
        for
          seq <- Ref.of[F, Int](0)
          captureFib <- handle.frames
            .evalMap { b64 =>
              if loopback then handle.receive("you (test)", b64)
              else seq.getAndUpdate(_ + 1).flatMap(n => outgoingQueue.offer(s"VOICE:$n:$b64"))
            }
            .compile
            .drain
            .start
          playbackFib <- handle.playback.compile.drain.start
          // Readers first: releasing the device under a blocked read throws.
          teardown = playbackFib.cancel *> captureFib.cancel *> release
          _ <- voiceRef.set(Some(VoiceSession(handle, teardown)))
          _ <- state.update(_.copy(inVoice = true, muted = false))
          _ <- if loopback then ().pure[F] else outgoingQueue.offer("VOICEJOIN")
          _ <- ui.printLine(
            if loopback then
              s"Voice self-test on (audio: ${handle.backend}). Speak and you should hear " +
                "yourself back. Use headphones (open speakers will feed back). " +
                "/mute or /voicetest to stop."
            else
              s"Joined voice (audio: ${handle.backend}). Use headphones to avoid echo. " +
                "/mute toggles your mic, /voice leaves."
          )
        yield ()
      }
    }.rescue { err =>
      ui.printLine(s"Voice unavailable: ${err.message}. Staying in text mode.")
    }

  private def stop(
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit] =
    voiceRef.getAndSet(None).flatMap {
      case None => ().pure[F]
      case Some(voice) =>
        state.update(_.copy(inVoice = false, muted = false, voiceUsers = Nil)) *>
          outgoingQueue.offer("VOICELEAVE") *>
          voice.teardown *>
          ui.setVoiceUsers(Nil) *>
          ui.printLine("Left voice.")
    }

  override def toggleMute(
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit] =
    voiceRef.get.flatMap {
      case None => ui.printLine("You're not in voice. Join with /voice first.")
      case Some(_) =>
        state.get.flatMap { st =>
          if st.adminMuted then ui.printLine("You are muted in voice by an admin.")
          else
            state
              .updateAndGet(s => s.copy(muted = !s.muted))
              .flatMap(s => ui.printLine(if s.muted then "Mic muted." else "Mic unmuted."))
        }
    }

object LiveVoice:
  def apply[F[_]: Concurrent](audio: Audio[F]): Voice[F] = new LiveVoice[F](audio)
