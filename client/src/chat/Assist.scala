package chat

import cats.effect.*
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Chunk
import fs2.Stream
import fs2.io.process.Process
import fs2.io.process.ProcessBuilder
import fs2.io.process.Processes
import org.typelevel.log4cats.LoggerFactory

import java.util.Base64

import Ansi.*

final case class AssistSession[F[_]](stdinQueue: Queue[F, Chunk[Byte]], teardown: F[Unit])

/** Consent-gated remote shell an admin can attach to (used by Emacs/TRAMP over the chat socket). */
trait Assist[F[_]]:
  def consentRequest(
      msg: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit]

  def answerConsent(
      approve: Boolean,
      raw: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit]

  def start(
      msg: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit]

  def data(msg: String, state: Ref[F, ClientState[F]]): F[Unit]

  def end(msg: String, state: Ref[F, ClientState[F]], ui: Ui[F]): F[Unit]

final class LiveAssist[F[_]: Async: Processes: LoggerFactory] private (
    notifications: Notifications[F],
    emoji: Emoji
) extends Assist[F]:
  private val logger = LoggerFactory[F].getLogger

  override def consentRequest(
      msg: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit] =
    msg.split(":", 3) match
      case Array(_, id, adminName) =>
        if Config.noAssist then
          ui.printLine(
            s"$serverColor⚠ Refused Emacs assist from $adminName (MUGGE_NO_ASSIST).$ansiReset"
          ) *> outgoingQueue.offer(s"ASSISTDENY:$id")
        else
          state.update(st => st.copy(pendingAssist = st.pendingAssist :+ (id, adminName))) *>
            ui.printLine(
              s"$serverColor⚠ Admin $adminName wants to connect to your machine (Emacs assist). " +
                s"Type yes to allow or no to deny.$ansiReset"
            ) *>
            notifications.send(
              title = "⚠ Admin assist request",
              body = s"$adminName wants to connect to your machine — answer yes or no in the chat",
              urgency = "critical",
              timeout = 0
            )
      case _ => ().pure[F]

  override def answerConsent(
      approve: Boolean,
      raw: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit] =
    state
      .modify { st =>
        st.pendingAssist match
          case answered :: rest => (st.copy(pendingAssist = rest), Some(answered))
          case Nil              => (st, None)
      }
      .flatMap {
        case None => outgoingQueue.offer(emoji.expand(raw))
        case Some((id, adminName)) =>
          if approve then
            outgoingQueue.offer(s"ASSISTACCEPT:$id") *>
              ui.printLine(s"${serverColor}Approved Emacs assist from $adminName.$ansiReset")
          else
            outgoingQueue.offer(s"ASSISTDENY:$id") *>
              ui.printLine(s"${serverColor}Denied Emacs assist from $adminName.$ansiReset")
      }

  override def start(
      msg: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit] =
    msg.split(":", 3) match
      case Array(_, id, adminName) =>
        if Config.noAssist then
          ui.printLine(
            s"$serverColor⚠ Refused Emacs assist from $adminName (MUGGE_NO_ASSIST).$ansiReset"
          ) *> outgoingQueue.offer(s"ASSISTEND:$id:refused")
        else spawnShell(id, adminName, state, outgoingQueue, ui)
      case _ => ().pure[F]

  private def spawnShell(
      id: String,
      adminName: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit] =
    for
      _ <- ui.printLine(
        s"$serverColor⚠ Admin $adminName has connected to your machine (Emacs assist)$ansiReset"
      )
      _ <- notifications.send(
        title = "⚠ Admin assist",
        body = s"$adminName connected to your machine (Emacs assist)",
        urgency = "critical",
        timeout = 0
      )
      spawned <- ProcessBuilder("script", "-qfc", "/bin/sh -i", "/dev/null")
        .withExtraEnv(Map("SHELL" -> "/bin/sh"))
        .spawn[F]
        .allocated
        .attempt
      _ <- spawned match
        case Left(err) =>
          logger.warn(s"Assist shell spawn failed: ${err.getMessage}") *>
            outgoingQueue.offer(s"ASSISTEND:$id:spawn-failed") *>
            ui.printLine(s"${serverColor}Admin assist session failed to start.$ansiReset")
        case Right((process, release)) =>
          pumpAssistShell(id, process, release, state, outgoingQueue, ui)
    yield ()

  private def pumpAssistShell(
      id: String,
      process: Process[F],
      release: F[Unit],
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit] =
    for
      stdinQ <- Queue.unbounded[F, Chunk[Byte]]
      inFib <- Stream
        .fromQueueUnterminated(stdinQ)
        .flatMap(Stream.chunk)
        .through(process.stdin)
        .compile
        .drain
        .attempt
        .void
        .start
      outFib <- process.stdout
        .merge(process.stderr)
        .chunks
        .evalMap { chunk =>
          outgoingQueue.offer(s"ASSISTDATA:$id:${Base64.getEncoder.encodeToString(chunk.toArray)}")
        }
        .compile
        .drain
        .flatMap(_ =>
          outgoingQueue.offer(s"ASSISTEND:$id:exited") *>
            ui.printLine(s"${serverColor}Admin assist session ended.$ansiReset")
        )
        .guarantee(
          state.update(st => st.copy(assistSessions = st.assistSessions - id)) *>
            inFib.cancel *> release.attempt.void
        )
        .start
      teardown = outFib.cancel *> inFib.cancel *> release.attempt.void
      _ <- state.update(st =>
        st.copy(assistSessions = st.assistSessions + (id -> AssistSession(stdinQ, teardown)))
      )
    yield ()

  override def data(msg: String, state: Ref[F, ClientState[F]]): F[Unit] =
    msg.split(":", 3) match
      case Array(_, id, b64) =>
        state.get.map(_.assistSessions.get(id)).flatMap {
          case None => ().pure[F]
          case Some(sess) =>
            Sync[F]
              .delay(Base64.getDecoder.decode(b64))
              .flatMap(bytes => sess.stdinQueue.offer(Chunk.array(bytes)))
        }
      case _ => ().pure[F]

  override def end(msg: String, state: Ref[F, ClientState[F]], ui: Ui[F]): F[Unit] =
    val id = msg.split(":", 3).lift(1).getOrElse("")
    state
      .modify { st =>
        val pending = st.pendingAssist.find(_._1 == id)
        val updated = st.copy(
          assistSessions = st.assistSessions - id,
          pendingAssist = st.pendingAssist.filterNot(_._1 == id)
        )
        (updated, (st.assistSessions.get(id), pending))
      }
      .flatMap {
        case (Some(sess), _) =>
          sess.teardown *> ui.printLine(s"${serverColor}Admin assist session ended.$ansiReset")
        case (None, Some((_, adminName))) =>
          ui.printLine(
            s"${serverColor}Assist request from $adminName was cancelled.$ansiReset"
          )
        case _ => ().pure[F]
      }

object LiveAssist:
  def apply[F[_]: Async: Processes: LoggerFactory](
      notifications: Notifications[F],
      emoji: Emoji
  ): Assist[F] = new LiveAssist[F](notifications, emoji)
