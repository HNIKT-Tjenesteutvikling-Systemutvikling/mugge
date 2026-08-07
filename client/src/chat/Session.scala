package chat

import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.Mutex
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.mtl.Handle.allow
import cats.syntax.all.*
import fs2.*
import fs2.io.net.*
import org.typelevel.log4cats.LoggerFactory

import scala.concurrent.duration.*

import Ansi.*

enum SessionOutcome:
  case Quit, Incompatible, Denied, Lost, ConnectFailed

/** One connected chat session: wires the socket, the keyboard and the timers together and reports
  * how it ended.
  */
trait Session[F[_]]:
  def run(
      socket: Socket[F],
      state: Ref[F, ClientState[F]],
      inputHistory: Ref[F, InputHistory],
      ipc: Ipc[F]
  ): F[SessionOutcome]

final class LiveSession[F[_]: Async: Console: LoggerFactory] private (
    config: Config[F],
    authentication: Authentication[F],
    terminal: Terminal[F],
    idle: Idle[F],
    userInput: UserInput[F],
    notifications: Notifications[F],
    whisper: Whisper[F],
    fileTransfer: FileTransfer[F],
    assist: Assist[F],
    markup: Markup,
    ansi: Ansi,
    highlighter: Highlighter
) extends Session[F]:
  private val logger = LoggerFactory[F].getLogger

  private val resizePollInterval = 300.milliseconds

  private val watchdogInterval = 30.seconds
  private val deadAfter = 3.minutes

  private val typingRefreshInterval = 3.seconds

  private val awayAfter = 10.minutes
  private val awayStatus = "away"

  private val suspendGraceThreshold = 30.seconds

  override def run(
      socket: Socket[F],
      state: Ref[F, ClientState[F]],
      inputHistory: Ref[F, InputHistory],
      ipc: Ipc[F]
  ): F[SessionOutcome] = {
    val initialData: F[String] = for {
      hostname <- config.hostname
      currentState <- state.get
      privateKey <- currentState.githubUsername.flatTraverse { _ =>
        allow[Authentication.AuthError] {
          authentication.loadPrivateKey().map(_.some)
        }.rescue { err =>
          logger.warn(s"Could not load SSH private key: ${err.message}").as(None)
        }.handleErrorWith { err =>
          logger.error(s"Could not load SSH private key: ${err.getMessage}").as(None)
        }
      }
      _ <- privateKey.traverse_(key => state.update(_.copy(privateKey = Some(key))))
      authData = currentState.githubUsername
        .filter(_ => privateKey.isDefined)
        .map(ghu => s"auto-auth:$ghu")
        .getOrElse("")
      finalString = List(hostname, s"proto:${Config.protocolVersion}", authData)
        .filter(_.nonEmpty)
        .mkString("\n") + "\n\n"
      _ <- logger.debug(s"Prepared initial data to send.")
    } yield finalString

    (
      terminal.size,
      Sync[F].delay(Option(System.console()).isDefined),
      Mutex[F],
      Queue.bounded[F, String](1024),
      Deferred[F, Either[Throwable, Unit]],
      Clock[F].monotonic.flatMap(t => Ref.of[F, FiniteDuration](t)),
      Clock[F].realTime.flatMap(t => Ref.of[F, FiniteDuration](t)),
      Ref.of[F, Boolean](false),
      Ref.of[F, Boolean](false),
      Ref.of[F, Boolean](false),
      Ref.of[F, String](""),
      Ref.of[F, Boolean](false),
      Ref.of[F, Int](0),
      Ref.of[F, Option[(Int, Int)]](None),
      Ref.of[F, Option[VoiceSession[F]]](None),
      Ref.of[F, Option[String]](None),
      Ref.of[F, Option[PendingPaste]](None),
      Ref.of[F, Option[(List[Char], Int)]](None),
      Ref.of[F, Vector[String]](Vector.empty)
    ).tupled.flatMap { tup =>
      val (
        initialSize,
        pty,
        mutex,
        outgoingQueue,
        halt,
        lastReceived,
        lastWatchdogWall,
        connectionLost,
        incompatible,
        denied,
        input,
        composing,
        blockLines,
        termSize,
        voiceRef,
        hint,
        pendingPaste,
        pasteBuf,
        scrollback
      ) = tup
      val ictl = InputCtl[F](input, hint, pendingPaste, pasteBuf, composing, inputHistory)
      val ui = LiveUi[F](
        mutex,
        state,
        ictl,
        blockLines,
        pty,
        termSize,
        scrollback,
        ansi,
        highlighter,
        terminal
      )

      val serverWriter: Stream[F, Nothing] =
        (Stream.eval(initialData) ++ Stream.fromQueueUnterminated(outgoingQueue))
          .map(_ + "\n")
          .evalTap {
            case data if controlNoise(data.trim) => ().pure[F]
            case data if sensitiveOutbound(data.trim) =>
              logger.debug("Writing to server: <redacted sensitive line>")
            case data => logger.debug(s"Writing to server: $data")
          }
          .through(text.utf8.encode)
          .through(socket.writes)
          .onFinalize(logger.debug("Server writer stream finished."))

      val serverReader: Stream[F, Unit] =
        readFromServer(
          socket,
          state,
          outgoingQueue,
          ui,
          lastReceived,
          halt,
          incompatible,
          denied,
          voiceRef,
          ipc
        )
          .onFinalize(logger.debug("Server reader stream finished."))

      val userReader: Stream[F, Unit] =
        userInput
          .readFromUser(outgoingQueue, halt, state, ui, ictl, voiceRef)
          .onFinalize(logger.debug("User reader stream finished."))

      val pinger: Stream[F, Unit] =
        Stream
          .awakeEvery[F](Config.pingInterval)
          .evalMap(_ => outgoingQueue.offer("PING"))

      val typingRefresher: Stream[F, Unit] =
        Stream.awakeEvery[F](typingRefreshInterval).evalMap { _ =>
          (composing.get, input.get).flatMapN { (c, inp) =>
            if c && inp.nonEmpty then outgoingQueue.offer("TYPING") else ().pure[F]
          }
        }

      val awayWatcher: Stream[F, Unit] =
        Stream.eval(Ref.of[F, AwayHold](None)).flatMap { held =>
          idle.transitions(awayAfter).evalMap(i => applyAway(i, held, state, outgoingQueue))
        }

      val watchdog: Stream[F, Unit] =
        Stream.awakeEvery[F](watchdogInterval).evalMap { _ =>
          for
            now <- Clock[F].monotonic
            wall <- Clock[F].realTime
            last <- lastReceived.get
            prevWall <- lastWatchdogWall.getAndSet(wall)
            resumed = (wall - prevWall) > (watchdogInterval + suspendGraceThreshold)
            _ <-
              if resumed then
                connectionLost.set(true) *>
                  ui.printLine("Resumed from suspend — reconnecting...") *>
                  halt.complete(Right(())).void
              else if (now - last) > deadAfter then
                connectionLost.set(true) *>
                  ui.printLine("Connection lost: no response from server.") *>
                  halt.complete(Right(())).void
              else ().pure[F]
          yield ()
        }

      val terminalWatcher: Stream[F, Unit] =
        if !pty then Stream.empty.covary[F]
        else
          Stream.eval(Ref.of[F, Option[(Int, Int)]](None)).flatMap { lastPoll =>
            Stream.awakeEvery[F](resizePollInterval).evalMap { _ =>
              terminal.size.flatMap { latest =>
                (lastPoll.getAndSet(latest), termSize.get).flatMapN { (prevPoll, committed) =>
                  val rearm =
                    if prevPoll.isEmpty && latest.isDefined then
                      Console[F].print(Terminal.enableInputModes)
                    else ().pure[F]
                  val apply =
                    if latest == prevPoll && latest != committed then ui.redraw else ().pure[F]
                  rearm *> apply
                }
              }
            }
          }

      val streams =
        Stream(
          serverReader,
          serverWriter,
          userReader,
          pinger,
          watchdog,
          typingRefresher,
          terminalWatcher,
          awayWatcher
        )

      val ipcSession =
        ipc.bind(line => userInput.dispatchLine(line, outgoingQueue, halt, state, ui, voiceRef))

      (terminal.rawMode(pty) *> ipcSession).use { _ =>
        termSize.set(initialSize) >>
          state.get.flatMap(st => ipc.me(st.username)) >>
          logger.debug("Starting chat streams...") >>
          ui.refreshInput >>
          streams.parJoinUnbounded
            .interruptWhen(halt)
            .compile
            .drain
            .guarantee(
              voiceRef.getAndSet(None).flatMap(_.traverse_(_.teardown)) *>
                state
                  .modify(st =>
                    (st.copy(assistSessions = Map.empty), st.assistSessions.values.toList)
                  )
                  .flatMap(_.traverse_(_.teardown))
            )
            .handleErrorWith { err =>
              if Tls.isPinMismatch(err) then
                denied.set(true) *> ui.printLine(Config.pinMismatchNotice)
              else
                connectionLost.set(true) *>
                  logger
                    .error(s"\nConnection error: ${Option(err.getMessage).getOrElse(err.toString)}")
            } >>
          (connectionLost.get, incompatible.get, denied.get).mapN { (lost, incompat, refused) =>
            if refused then SessionOutcome.Denied
            else if incompat then SessionOutcome.Incompatible
            else if lost then SessionOutcome.Lost
            else SessionOutcome.Quit
          }
      }
    }
  }

  private def controlNoise(line: String): Boolean =
    line == "PING" || line == "TYPING" || line == "TYPINGSTOP" ||
      line == "VOICEJOIN" || line == "VOICELEAVE" || line.startsWith("VOICE:")

  private def sensitiveOutbound(line: String): Boolean =
    line.startsWith("SIGNATURE:") ||
      line.startsWith("FILEDATA:") || line.startsWith("FILEEND:") ||
      line.startsWith("ASSISTDATA:")

  private def readFromServer(
      socket: Socket[F],
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F],
      lastReceived: Ref[F, FiniteDuration],
      halt: Deferred[F, Either[Throwable, Unit]],
      incompatible: Ref[F, Boolean],
      denied: Ref[F, Boolean],
      voiceRef: Ref[F, Option[VoiceSession[F]]],
      ipc: Ipc[F]
  ): Stream[F, Nothing] =
    Stream.eval(Ref.of[F, Map[String, CodeAccum]](Map.empty)).flatMap { codeAccum =>
      socket.reads
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(_.nonEmpty)
        .evalMap { msg =>
          Clock[F].monotonic.flatMap(lastReceived.set) *> state.get.flatMap { st =>
            val me = st.username
            if msg == "PONG" then ().pure[F]
            else if msg.startsWith("INCOMPATIBLE:") then
              incompatible.set(true) *>
                ui.printLine(msg.drop("INCOMPATIBLE:".length)) *>
                halt.complete(Right(())).void
            else if msg.startsWith("DENIED:") then
              denied.set(true) *>
                ui.printLine(msg.drop("DENIED:".length)) *>
                halt.complete(Right(())).void
            else if msg.startsWith("CHALLENGE:") then
              handleAutoChallenge(msg.drop(10), state, outgoingQueue)
            else if msg == "ADMIN" then state.update(_.copy(isAdmin = true))
            else if msg.startsWith("MUTED:") then
              val muted = msg.drop("MUTED:".length).trim == "1"
              state.update(_.copy(adminMuted = muted))
            else if msg.startsWith("NICK:") then
              val newName = msg.drop("NICK:".length).trim
              state.update(_.copy(username = newName)) *>
                ipc.me(newName) *>
                ui.printLine(s"${serverColor}You are now known as $newName$ansiReset")
            else if msg.startsWith("HIST:") then
              handleIncomingChat(
                msg.drop("HIST:".length),
                me,
                state,
                ui,
                codeAccum,
                ipc,
                live = false
              )
            else if msg.startsWith("STATUS:") then
              msg.split(":", 3) match
                case Array(_, name, text) =>
                  val trimmed = text.trim
                  ui.setStatuses(name.trim, if trimmed.isEmpty then None else Some(trimmed))
                case _ => ().pure[F]
            else if msg.startsWith("USERS:") then
              val users = msg.drop(6).split(",").map(_.trim).filter(_.nonEmpty).toList
              ui.setUsers(users) *> ipc.users(users)
            else if msg.startsWith("TYPING:") then
              val users = msg
                .drop(7)
                .split(",")
                .map(_.trim)
                .filter(_.nonEmpty)
                .filterNot(_.equalsIgnoreCase(me))
                .toList
              ui.setTyping(users) *> ipc.typing(users)
            else if msg.startsWith("VOICEUSERS:") then
              val users =
                msg.drop("VOICEUSERS:".length).split(",").map(_.trim).filter(_.nonEmpty).toList
              ui.setVoiceUsers(users)
            else if msg.startsWith("VOICE:") then
              msg.split(":", 4) match
                case Array(_, from, _, b64) =>
                  voiceRef.get.flatMap(_.traverse_(_.handle.receive(from, b64)))
                case _ => ().pure[F]
            else if msg.startsWith("NUDGE:") then notifications.nudge(msg, state)
            else if msg.startsWith("WHISPERTO:") then whisper.outgoing(msg, ui, ipc)
            else if msg.startsWith("WHISPER:") then whisper.incoming(msg, ui, ipc)
            else if msg.startsWith("REMIND:") then notifications.reminder(msg, me, ui)
            else if msg.startsWith("FILEOFFER:") then fileTransfer.offer(msg, state, ui)
            else if msg.startsWith("FILEACCEPT:") then
              fileTransfer.accept(msg.drop(11).trim, state, outgoingQueue, ui)
            else if msg.startsWith("FILEREJECT:") then
              fileTransfer.reject(msg.drop(11).trim, state, ui)
            else if msg.startsWith("FILEDATA:") then fileTransfer.data(msg, state, ui)
            else if msg.startsWith("FILEEND:") then fileTransfer.end(msg, state, ui)
            else if msg.startsWith("ASSISTDATA:") then assist.data(msg, state)
            else if msg.startsWith("ASSISTEND:") then assist.end(msg, state, ui)
            else if msg.startsWith("ASSISTREQ:") then
              assist.consentRequest(msg, state, outgoingQueue, ui)
            else if msg.startsWith("ASSIST:") then assist.start(msg, state, outgoingQueue, ui)
            else handleIncomingChat(msg, me, state, ui, codeAccum, ipc)
          }
        }
        .drain
    }

  private def handleIncomingChat(
      msg: String,
      me: String,
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      codeAccum: Ref[F, Map[String, CodeAccum]],
      ipc: Ipc[F],
      live: Boolean = true
  ): F[Unit] =
    def publish: F[Unit] =
      msg match
        case Markup.displayPattern(time, indicator, sender, content) =>
          ipc.message(time, indicator, sender.trim, content, history = !live)
        case _ => ipc.notice(msg)

    def plain: F[Unit] =
      publish >>
        (if live then
           ui.colorize(msg, state).flatMap(ui.printLine) >> notifications.mentions(msg, me)
         else ui.colorize(msg, state, withStatus = false).flatMap(ui.printLine))

    msg match
      case Markup.displayPattern(time, indicator, senderRaw, content) =>
        val sender = senderRaw.trim
        codeAccum.get.flatMap { accums =>
          accums.get(sender) match
            case Some(acc) if content.startsWith("│") =>
              val body = acc.body :+ content.stripPrefix("│ ").stripPrefix("│")
              if acc.remaining <= 1 then
                codeAccum.update(_ - sender) *>
                  renderCodeBody(time, indicator, sender, body, ui, ipc, history = !live)
              else codeAccum.update(_ + (sender -> CodeAccum(acc.remaining - 1, body)))
            case _ =>
              content match
                case Markup.codeHeaderPattern(n) =>
                  n.toIntOption.filter(_ > 0) match
                    case Some(count) =>
                      codeAccum.update(_ + (sender -> CodeAccum(count, Vector.empty)))
                    case None => plain
                case _ =>
                  markup.inlineCode(content) match
                    case Some(code) =>
                      renderCodeBody(
                        time,
                        indicator,
                        sender,
                        Vector(code),
                        ui,
                        ipc,
                        history = !live
                      )
                    case None => plain
        }
      case _ => plain

  private def renderCodeBody(
      time: String,
      indicator: String,
      sender: String,
      body: Vector[String],
      ui: Ui[F],
      ipc: Ipc[F],
      history: Boolean
  ): F[Unit] =
    val (lang, code) = body.headOption.flatMap(markup.fenceLang) match
      case Some(l) => (l, body.drop(1).dropRight(1).toList)
      case None    => ("", body.toList)
    ipc.codeMessage(time, indicator, sender, lang, code, history) *>
      ui.printCodeBlock(time, indicator, sender, lang, code)

  private def handleAutoChallenge(
      challenge: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String]
  ): F[Unit] =
    for {
      currentState <- state.get
      _ <- (currentState.privateKey, currentState.githubUsername) match {
        case (Some(privateKey), Some(githubUsername)) =>
          allow[Authentication.AuthError] {
            for {
              _ <- logger.debug(s"Received auto-auth challenge, signing for '$githubUsername'...")
              signature <- authentication.signChallenge(challenge, privateKey)
              _ <- outgoingQueue.offer(s"SIGNATURE:$signature")
              _ <- logger.debug("Auto-authentication response sent to queue.")
            } yield ()
          }.rescue { err =>
            logger.warn(s"Could not sign auto-auth challenge: ${err.message}")
          }

        case _ =>
          logger.debug("Cannot auto-authenticate: missing private key or GitHub username.")
      }
    } yield ()

  private type AwayHold = Option[Option[String]]

  private def applyAway(
      idleNow: Boolean,
      held: Ref[F, AwayHold],
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String]
  ): F[Unit] =
    (state.get, held.get).flatMapN { (st, hold) =>
      (hold, idleNow) match
        case (None, true) if !st.inVoice =>
          held.set(Some(st.statuses.get(st.username))) *>
            outgoingQueue.offer(s"/status $awayStatus")
        case (Some(previous), false) =>
          held.set(None) *>
            outgoingQueue.offer(previous.fold("/status")(text => s"/status $text"))
        case _ => ().pure[F]
    }

object LiveSession:
  def apply[F[_]: Async: Console: LoggerFactory](
      config: Config[F],
      authentication: Authentication[F],
      terminal: Terminal[F],
      idle: Idle[F],
      userInput: UserInput[F],
      notifications: Notifications[F],
      whisper: Whisper[F],
      fileTransfer: FileTransfer[F],
      assist: Assist[F],
      markup: Markup,
      ansi: Ansi,
      highlighter: Highlighter
  ): Session[F] =
    new LiveSession[F](
      config,
      authentication,
      terminal,
      idle,
      userInput,
      notifications,
      whisper,
      fileTransfer,
      assist,
      markup,
      ansi,
      highlighter
    )
