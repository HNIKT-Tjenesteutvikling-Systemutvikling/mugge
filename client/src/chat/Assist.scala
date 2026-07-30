package chat

import cats.effect.*
import cats.effect.std.Queue
import fs2.Chunk
import fs2.Stream
import fs2.io.process.ProcessBuilder
import org.typelevel.log4cats.Logger as TLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.util.Base64

import Ansi.*

final case class AssistSession(stdinQueue: Queue[IO, Chunk[Byte]], teardown: IO[Unit])

/** Consent-gated remote shell an admin can attach to (used by Emacs/TRAMP over the chat socket). */
object Assist:
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  def consentRequest(
      msg: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): IO[Unit] =
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
            Notifications.send(
              title = "⚠ Admin assist request",
              body = s"$adminName wants to connect to your machine — answer yes or no in the chat",
              urgency = "critical",
              timeout = 0
            )
      case _ => IO.unit

  def answerConsent(
      approve: Boolean,
      raw: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): IO[Unit] =
    state
      .modify { st =>
        st.pendingAssist match
          case answered :: rest => (st.copy(pendingAssist = rest), Some(answered))
          case Nil              => (st, None)
      }
      .flatMap {
        // No pending request: yes/no is just chat.
        case None => outgoingQueue.offer(Emoji.expand(raw))
        case Some((id, adminName)) =>
          if approve then
            outgoingQueue.offer(s"ASSISTACCEPT:$id") *>
              ui.printLine(s"${serverColor}Approved Emacs assist from $adminName.$ansiReset")
          else
            outgoingQueue.offer(s"ASSISTDENY:$id") *>
              ui.printLine(s"${serverColor}Denied Emacs assist from $adminName.$ansiReset")
      }

  def start(
      msg: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): IO[Unit] =
    msg.split(":", 3) match
      case Array(_, id, adminName) =>
        if Config.noAssist then
          ui.printLine(
            s"$serverColor⚠ Refused Emacs assist from $adminName (MUGGE_NO_ASSIST).$ansiReset"
          ) *> outgoingQueue.offer(s"ASSISTEND:$id:refused")
        else spawnShell(id, adminName, state, outgoingQueue, ui)
      case _ => IO.unit

  private def spawnShell(
      id: String,
      adminName: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): IO[Unit] =
    for
      _ <- ui.printLine(
        s"$serverColor⚠ Admin $adminName has connected to your machine (Emacs assist)$ansiReset"
      )
      _ <- Notifications.send(
        title = "⚠ Admin assist",
        body = s"$adminName connected to your machine (Emacs assist)",
        urgency = "critical",
        timeout = 0
      )
      // pty via `script`: TRAMP hangs on a piped /bin/sh (no prompt, no stty).
      // SHELL pinned because `script -c` runs the command via $SHELL (fish here).
      spawned <- ProcessBuilder("script", "-qfc", "/bin/sh -i", "/dev/null")
        .withExtraEnv(Map("SHELL" -> "/bin/sh"))
        .spawn[IO]
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
      process: fs2.io.process.Process[IO],
      release: IO[Unit],
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): IO[Unit] =
    for
      stdinQ <- Queue.unbounded[IO, Chunk[Byte]]
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

  def data(msg: String, state: Ref[IO, ClientState]): IO[Unit] =
    msg.split(":", 3) match
      case Array(_, id, b64) =>
        state.get.map(_.assistSessions.get(id)).flatMap {
          case None => IO.unit
          case Some(sess) =>
            IO(Base64.getDecoder.decode(b64)).flatMap(bytes =>
              sess.stdinQueue.offer(Chunk.array(bytes))
            )
        }
      case _ => IO.unit

  def end(msg: String, state: Ref[IO, ClientState], ui: Ui): IO[Unit] =
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
        case _ => IO.unit
      }
