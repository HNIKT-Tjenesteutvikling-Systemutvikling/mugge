package chat

import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.Mutex
import cats.effect.std.Queue
import cats.mtl.Handle.allow
import cats.syntax.all.*
import fs2.*
import fs2.io.net.*
import org.typelevel.log4cats.Logger as TLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

import Ansi.*

enum SessionOutcome:
  case Quit, Incompatible, Denied, Lost, ConnectFailed

/** One connected chat session: wires the socket, the keyboard and the timers together and reports
  * how it ended.
  */
object Session:
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  private val resizePollInterval = 300.milliseconds

  private val watchdogInterval = 30.seconds
  private val deadAfter = 3.minutes

  private val typingRefreshInterval = 3.seconds

  private val awayAfter = 10.minutes
  private val awayStatus = "away"

  private val suspendGraceThreshold = 30.seconds

  def run(
      socket: Socket[IO],
      state: Ref[IO, ClientState],
      inputHistory: Ref[IO, InputHistory]
  ): IO[SessionOutcome] = {
    val initialDataIO: IO[String] = for {
      hostname <- Config.hostname
      currentState <- state.get
      privateKey <- currentState.githubUsername.flatTraverse { _ =>
        allow[Authentication.AuthError] {
          Authentication.loadPrivateKey().map(_.some)
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
      Terminal.size,
      IO(Option(System.console()).isDefined),
      Mutex[IO],
      Queue.bounded[IO, String](1024),
      Deferred[IO, Either[Throwable, Unit]],
      IO.monotonic.flatMap(t => Ref.of[IO, FiniteDuration](t)),
      IO.realTime.flatMap(t => Ref.of[IO, FiniteDuration](t)),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, String](""),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, Int](0),
      Ref.of[IO, Option[(Int, Int)]](None),
      Ref.of[IO, Option[Voice]](None),
      Ref.of[IO, Option[String]](None),
      Ref.of[IO, Option[PendingPaste]](None),
      Ref.of[IO, Option[(List[Char], Int)]](None),
      Ref.of[IO, Vector[String]](Vector.empty)
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
      val ictl = InputCtl(input, hint, pendingPaste, pasteBuf, composing, inputHistory)
      val ui = new Ui(mutex, state, ictl, blockLines, pty, termSize, scrollback)

      val serverWriter: Stream[IO, Nothing] =
        (Stream.eval(initialDataIO) ++ Stream.fromQueueUnterminated(outgoingQueue))
          .map(_ + "\n")
          .evalTap {
            case data if controlNoise(data.trim) => IO.unit
            case data if sensitiveOutbound(data.trim) =>
              logger.debug("Writing to server: <redacted sensitive line>")
            case data => logger.debug(s"Writing to server: $data")
          }
          .through(text.utf8.encode)
          .through(socket.writes)
          .onFinalize(logger.debug("Server writer stream finished."))

      val serverReader: Stream[IO, Unit] =
        readFromServer(
          socket,
          state,
          outgoingQueue,
          ui,
          lastReceived,
          halt,
          incompatible,
          denied,
          voiceRef
        )
          .onFinalize(logger.debug("Server reader stream finished."))

      val userReader: Stream[IO, Unit] =
        UserInput
          .readFromUser(outgoingQueue, halt, state, ui, ictl, voiceRef)
          .onFinalize(logger.debug("User reader stream finished."))

      val pinger: Stream[IO, Unit] =
        Stream
          .awakeEvery[IO](Config.pingInterval)
          .evalMap(_ => outgoingQueue.offer("PING"))

      val typingRefresher: Stream[IO, Unit] =
        Stream.awakeEvery[IO](typingRefreshInterval).evalMap { _ =>
          (composing.get, input.get).flatMapN { (c, inp) =>
            if c && inp.nonEmpty then outgoingQueue.offer("TYPING") else IO.unit
          }
        }

      val awayWatcher: Stream[IO, Unit] =
        Stream.eval(Ref.of[IO, AwayHold](None)).flatMap { held =>
          Idle.transitions(awayAfter).evalMap(idle => applyAway(idle, held, state, outgoingQueue))
        }

      val watchdog: Stream[IO, Unit] =
        Stream.awakeEvery[IO](watchdogInterval).evalMap { _ =>
          for
            now <- IO.monotonic
            wall <- IO.realTime
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
              else IO.unit
          yield ()
        }

      val terminalWatcher: Stream[IO, Unit] =
        if !pty then Stream.empty.covary[IO]
        else
          Stream.eval(Ref.of[IO, Option[(Int, Int)]](None)).flatMap { lastPoll =>
            Stream.awakeEvery[IO](resizePollInterval).evalMap { _ =>
              Terminal.size.flatMap { latest =>
                (lastPoll.getAndSet(latest), termSize.get).flatMapN { (prevPoll, committed) =>
                  // Re-arm bracketed paste when the pty first appears.
                  val rearm =
                    if prevPoll.isEmpty && latest.isDefined then
                      Console[IO].print(Terminal.enableInputModes)
                    else IO.unit
                  // Debounce: repaint once two consecutive polls agree, so a
                  // drag-resize redraws once at the final size.
                  val apply =
                    if latest == prevPoll && latest != committed then ui.redraw else IO.unit
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

      Terminal.rawMode(pty).use { _ =>
        termSize.set(initialSize) >>
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
      socket: Socket[IO],
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui,
      lastReceived: Ref[IO, FiniteDuration],
      halt: Deferred[IO, Either[Throwable, Unit]],
      incompatible: Ref[IO, Boolean],
      denied: Ref[IO, Boolean],
      voiceRef: Ref[IO, Option[Voice]]
  ): Stream[IO, Nothing] =
    Stream.eval(Ref.of[IO, Map[String, CodeAccum]](Map.empty)).flatMap { codeAccum =>
      socket.reads
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(_.nonEmpty)
        .evalMap { msg =>
          IO.monotonic.flatMap(lastReceived.set) *> state.get.flatMap { st =>
            val me = st.username
            if msg == "PONG" then IO.unit
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
                ui.printLine(s"${serverColor}You are now known as $newName$ansiReset")
            // Replayed history renders like chat (code blocks included) but must
            // not re-fire mention/ping notifications or carry today's statuses.
            else if msg.startsWith("HIST:") then
              handleIncomingChat(msg.drop("HIST:".length), me, state, ui, codeAccum, live = false)
            else if msg.startsWith("STATUS:") then
              msg.split(":", 3) match
                case Array(_, name, text) =>
                  val trimmed = text.trim
                  ui.setStatuses(name.trim, if trimmed.isEmpty then None else Some(trimmed))
                case _ => IO.unit
            else if msg.startsWith("USERS:") then
              val users = msg.drop(6).split(",").map(_.trim).filter(_.nonEmpty).toList
              ui.setUsers(users)
            else if msg.startsWith("TYPING:") then
              val users = msg
                .drop(7)
                .split(",")
                .map(_.trim)
                .filter(_.nonEmpty)
                .filterNot(_.equalsIgnoreCase(me))
                .toList
              ui.setTyping(users)
            else if msg.startsWith("VOICEUSERS:") then
              val users =
                msg.drop("VOICEUSERS:".length).split(",").map(_.trim).filter(_.nonEmpty).toList
              ui.setVoiceUsers(users)
            else if msg.startsWith("VOICE:") then
              msg.split(":", 4) match
                case Array(_, from, _, b64) =>
                  voiceRef.get.flatMap(_.traverse_(_.handle.receive(from, b64)))
                case _ => IO.unit
            else if msg.startsWith("REMIND:") then Notifications.reminder(msg, me, ui)
            else if msg.startsWith("FILEOFFER:") then FileTransfer.offer(msg, state, ui)
            else if msg.startsWith("FILEACCEPT:") then
              FileTransfer.accept(msg.drop(11).trim, state, outgoingQueue, ui)
            else if msg.startsWith("FILEREJECT:") then
              FileTransfer.reject(msg.drop(11).trim, state, ui)
            else if msg.startsWith("FILEDATA:") then FileTransfer.data(msg, state, ui)
            else if msg.startsWith("FILEEND:") then FileTransfer.end(msg, state, ui)
            else if msg.startsWith("ASSISTDATA:") then Assist.data(msg, state)
            else if msg.startsWith("ASSISTEND:") then Assist.end(msg, state, ui)
            else if msg.startsWith("ASSISTREQ:") then
              Assist.consentRequest(msg, state, outgoingQueue, ui)
            else if msg.startsWith("ASSIST:") then Assist.start(msg, state, outgoingQueue, ui)
            else handleIncomingChat(msg, me, state, ui, codeAccum)
          }
        }
        .drain
    }

  private def handleIncomingChat(
      msg: String,
      me: String,
      state: Ref[IO, ClientState],
      ui: Ui,
      codeAccum: Ref[IO, Map[String, CodeAccum]],
      live: Boolean = true
  ): IO[Unit] =
    def plain: IO[Unit] =
      if live then
        Ui.colorize(msg, state).flatMap(ui.printLine) >>
          Notifications.mentions(msg, me) >>
          Notifications.pings(msg, me, state)
      else Ui.colorize(msg, state, withStatus = false).flatMap(ui.printLine)

    msg match
      case Markup.displayPattern(time, indicator, senderRaw, content) =>
        val sender = senderRaw.trim
        codeAccum.get.flatMap { accums =>
          accums.get(sender) match
            case Some(acc) if content.startsWith("│") =>
              val body = acc.body :+ content.stripPrefix("│ ").stripPrefix("│")
              if acc.remaining <= 1 then
                codeAccum.update(_ - sender) *>
                  renderCodeBody(time, indicator, sender, body, ui)
              else codeAccum.update(_ + (sender -> CodeAccum(acc.remaining - 1, body)))
            case _ =>
              content match
                case Markup.codeHeaderPattern(n) =>
                  n.toIntOption.filter(_ > 0) match
                    case Some(count) =>
                      codeAccum.update(_ + (sender -> CodeAccum(count, Vector.empty)))
                    case None => plain
                case _ =>
                  Markup.inlineCode(content) match
                    case Some(code) =>
                      renderCodeBody(time, indicator, sender, Vector(code), ui)
                    case None => plain
        }
      case _ => plain

  // `body` still carries the literal fences on a framed block; drop them and
  // take the lang from the opening fence. An inline snippet arrives fence-free.
  private def renderCodeBody(
      time: String,
      indicator: String,
      sender: String,
      body: Vector[String],
      ui: Ui
  ): IO[Unit] =
    val (lang, code) = body.headOption.flatMap(Markup.fenceLang) match
      case Some(l) => (l, body.drop(1).dropRight(1).toList)
      case None    => ("", body.toList)
    ui.printCodeBlock(time, indicator, sender, lang, code)

  private def handleAutoChallenge(
      challenge: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String]
  ): IO[Unit] =
    for {
      currentState <- state.get
      _ <- (currentState.privateKey, currentState.githubUsername) match {
        case (Some(privateKey), Some(githubUsername)) =>
          allow[Authentication.AuthError] {
            for {
              _ <- logger.debug(s"Received auto-auth challenge, signing for '$githubUsername'...")
              signature <- Authentication.signChallenge(challenge, privateKey)
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

  // Some(previous) exactly while the away override is ours to undo, so a status
  // the user set by hand is never overwritten or restored on their behalf.
  private type AwayHold = Option[Option[String]]

  private def applyAway(
      idle: Boolean,
      held: Ref[IO, AwayHold],
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String]
  ): IO[Unit] =
    (state.get, held.get).flatMapN { (st, hold) =>
      (hold, idle) match
        case (None, true) if !st.inVoice =>
          held.set(Some(st.statuses.get(st.username))) *>
            outgoingQueue.offer(s"/status $awayStatus")
        case (Some(previous), false) =>
          held.set(None) *>
            outgoingQueue.offer(previous.fold("/status")(text => s"/status $text"))
        case _ => IO.unit
    }
