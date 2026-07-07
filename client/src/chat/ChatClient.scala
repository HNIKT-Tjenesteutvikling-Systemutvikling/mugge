package chat

import cats.effect.*
import cats.effect.std.{Console, Mutex, Queue}
import cats.syntax.all.*
import fs2.*
import fs2.io.net.*
import fs2.io.file.{Files as Fs2Files, Path as Fs2Path}
import com.comcast.ip4s.*
import java.io.IOException
import scala.sys.process.*
import scala.concurrent.duration.*
import java.security.*
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.Base64
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger => TLogger}

object ChatClient extends IOApp:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  private val defaultPort = port"20222"
  private val defaultHost = host"localhost"

  // Wire-protocol version sent to the server on connect. Bump in BOTH repos
  // when a change makes older clients incompatible; the server refuses any
  // client below its required minimum with an update-and-rebuild message.
  private val protocolVersion = 1

  // Set by the systemd user service (task 8). In this mode the client is a
  // shared background process, so /quit must NOT exit (that would kill the
  // service for everyone). Leaving a terminal is a dtach action the client
  // can't perform itself, so we point the user at it instead.
  private val serviceMode: Boolean = sys.env.get("MUGGE_SERVICE").contains("1")

  private val quitHint =
    "/quit is disabled here — this chat runs in the background. Press Ctrl-\\ " +
      "(or just close the terminal) to leave without disconnecting. To stop it " +
      "entirely: systemctl --user stop mugge-chat"

  private val maxFileSize = 10L * 1024 * 1024 // 10 MiB
  private val fileChunkSize = 48 * 1024 // raw bytes per chunk before base64

  // Application-level heartbeat: keeps the TCP flow non-idle so Azure's load
  // balancer (~4 min idle drop) never silently kills a quiet connection, and
  // doubles as a server-process liveness probe. deadAfter spans ~3 PONGs.
  private val pingInterval = 60.seconds
  private val watchdogInterval = 30.seconds
  private val deadAfter = 3.minutes

  // Must stay under the server's ~5s typing lease so the indicator doesn't lapse.
  private val typingRefreshInterval = 3.seconds

  case class OutgoingFile(path: Path, name: String, size: Long)

  case class IncomingFile(from: String, name: String, size: Long, temp: Option[Path] = None)

  case class ClientState(
      authenticated: Boolean = false,
      githubUsername: Option[String] = None,
      privateKey: Option[PrivateKey] = None,
      colors: Map[String, Int] = Map.empty,
      onlineUsers: List[String] = Nil,
      typingUsers: List[String] = Nil,
      outgoingFiles: Map[String, OutgoingFile] = Map.empty,
      incomingFiles: Map[String, IncomingFile] = Map.empty,
      pingHistory: Map[String, List[FiniteDuration]] = Map.empty
  )

  private val ansiReset = "\u001b[0m"

  private val ansiPalette: Vector[String] = Vector(
    "\u001b[38;5;39m",
    "\u001b[38;5;208m",
    "\u001b[38;5;46m",
    "\u001b[38;5;201m",
    "\u001b[38;5;226m",
    "\u001b[38;5;51m",
    "\u001b[38;5;196m",
    "\u001b[38;5;129m",
    "\u001b[38;5;118m",
    "\u001b[38;5;214m",
    "\u001b[38;5;45m",
    "\u001b[38;5;213m"
  )

  private val ansiDimPalette: Vector[String] = Vector(
    "\u001b[38;5;25m",
    "\u001b[38;5;130m",
    "\u001b[38;5;28m",
    "\u001b[38;5;90m",
    "\u001b[38;5;100m",
    "\u001b[38;5;30m",
    "\u001b[38;5;88m",
    "\u001b[38;5;54m",
    "\u001b[38;5;64m",
    "\u001b[38;5;136m",
    "\u001b[38;5;24m",
    "\u001b[38;5;96m"
  )

  private val serverColor = "\u001b[38;5;245m"

  private val displayPattern =
    """^\[(\d{2}:\d{2}:\d{2})\] ([✓?]) ([^:]+): (.*)$""".r

  // True only for a genuine server-authored line (clientId == SERVER); guards
  // the auth-success interception so a user can't spoof it by typing the phrase.
  private def isFromServer(msg: String): Boolean =
    msg match
      case displayPattern(_, _, sender, _) => sender.trim == "SERVER"
      case _                               => false

  private def colorIndexFor(name: String, state: Ref[IO, ClientState]): IO[Int] =
    state.modify { st =>
      st.colors.get(name) match
        case Some(idx) => (st, idx)
        case None =>
          val idx = st.colors.size % ansiPalette.size
          (st.copy(colors = st.colors + (name -> idx)), idx)
    }

  private def colorizeForDisplay(msg: String, state: Ref[IO, ClientState]): IO[String] =
    msg match
      case displayPattern(time, indicator, sender, content) =>
        if sender.trim == "SERVER" then
          IO.pure(
            s"[$time] $indicator $serverColor$sender$ansiReset: $serverColor$content$ansiReset"
          )
        else
          colorIndexFor(sender.trim, state).map { idx =>
            val bright = ansiPalette(idx)
            val dim = ansiDimPalette(idx)
            s"[$time] $indicator $bright$sender$ansiReset: $dim$content$ansiReset"
          }
      case _ => IO.pure(msg)

  private val panelWidth = 24

  // (cols, rows) when stdout is an interactive TTY, else None (piped/redirected).
  // Reads the size from the controlling terminal (/dev/tty) rather than `tput`,
  // whose result would be the 80-col default when our stdout is captured.
  private def detectTerminal: IO[Option[(Int, Int)]] =
    if System.console() == null then IO.pure(None)
    else
      IO.blocking(Seq("sh", "-c", "stty size < /dev/tty").!!.trim)
        .map { out =>
          out.split("\\s+").toList match
            case rows :: cols :: Nil =>
              // 0x0 => a headless pty (e.g. dtach -N with no client attached);
              // treat as no-TTY so we fall back to plain-line mode.
              (cols.toIntOption, rows.toIntOption).tupled.filter((c, r) => c > 0 && r > 0)
            case _ => None
        }
        .handleError(_ => None)

  private val inputPrompt = "> "

  private def formatTyping(users: List[String]): Option[String] =
    users match
      case Nil      => None
      case a :: Nil => Some(s"$a is typing...")
      case _ =>
        val init = users.init.mkString(", ")
        Some(s"$init and ${users.last} is typing...")

  final class Ui(
      mutex: Mutex[IO],
      state: Ref[IO, ClientState],
      input: Ref[IO, String],
      blockLines: Ref[IO, Int],
      pty: Boolean,
      termSize: Ref[IO, Option[(Int, Int)]]
  ):
    def isTty: Boolean = pty

    private def plainPrint(line: String): IO[Unit] =
      if pty then Console[IO].print(line + "\r\n") else Console[IO].println(line)

    def printLine(line: String): IO[Unit] =
      mutex.lock.surround {
        termSize.get.flatMap {
          case None               => plainPrint(line)
          case Some((cols, rows)) => render(Some(line), cols, rows)
        }
      }

    def setUsers(users: List[String]): IO[Unit] =
      state.modify(st => (st.copy(onlineUsers = users), st.onlineUsers != users)).flatMap {
        changed =>
          if !changed then IO.unit
          else
            mutex.lock.surround {
              termSize.get.flatMap {
                case None if pty        => IO.unit
                case None               => plainPrint(s"Online: ${users.mkString(", ")}")
                case Some((cols, rows)) => render(None, cols, rows)
              }
            }
      }

    def setTyping(users: List[String]): IO[Unit] =
      state.modify(st => (st.copy(typingUsers = users), st.typingUsers != users)).flatMap {
        changed =>
          if !changed then IO.unit
          else
            mutex.lock.surround {
              termSize.get.flatMap {
                case None               => IO.unit
                case Some((cols, rows)) => render(None, cols, rows)
              }
            }
      }

    def refreshInput: IO[Unit] =
      mutex.lock.surround {
        termSize.get.flatMap {
          case None               => IO.unit
          case Some((cols, rows)) => render(None, cols, rows)
        }
      }

    def onResize: IO[Unit] =
      mutex.lock.surround {
        termSize.get.flatMap {
          case None               => IO.unit
          case Some((cols, rows)) => blockLines.set(0) *> render(None, cols, rows)
        }
      }

    private def render(chat: Option[String], cols: Int, rows: Int): IO[Unit] =
      for
        st <- state.get
        inp <- input.get
        prev <- blockLines.get
        startCol = math.max(1, cols - panelWidth + 1)
        clearRows = math.min(rows - 1, 30)
        visible = st.onlineUsers.take(math.max(0, clearRows - 1))
        colored <- visible.traverse(u => colorIndexFor(u, state).map(idx => (u, ansiPalette(idx))))
        (blockStr, newCount) = renderBlock(st, inp)
        sb = new StringBuilder
        _ = sb.append(eraseBlock(prev))
        _ = chat.foreach(l => sb.append(l).append("\n"))
        _ = sb.append(blockStr)
        _ = sb.append(panelStr(colored, st.onlineUsers.size, startCol, clearRows))
        _ <- Console[IO].print(sb.toString)
        _ <- blockLines.set(newCount)
      yield ()

    private def eraseBlock(n: Int): String =
      if n <= 0 then ""
      else
        val up = if n > 1 then s"\u001b[${n - 1}A" else ""
        s"\r$up\u001b[0J"

    private def renderBlock(st: ClientState, inp: String): (String, Int) =
      val typingLine =
        formatTyping(st.typingUsers).map(t => s"\r\u001b[2K\u001b[2m$t$ansiReset\n")
      val inputLine = s"\r\u001b[2K$inputPrompt$inp"
      val count = (if typingLine.isDefined then 1 else 0) + 1
      (typingLine.getOrElse("") + inputLine, count)

    private def panelStr(
        colored: List[(String, String)],
        total: Int,
        startCol: Int,
        clearRows: Int
    ): String =
      val clears =
        (1 to clearRows).map(r => s"\u001b[$r;${startCol}H" + " " * panelWidth).mkString
      val header = s"\u001b[1;${startCol}H\u001b[1m\u2524 Online ($total)\u001b[0m"
      val entries = colored.zipWithIndex.map { case ((u, color), i) =>
        val name = if u.length > panelWidth - 2 then u.take(panelWidth - 2) else u
        s"\u001b[${2 + i};${startCol}H$color\u2022 $name$ansiReset"
      }.mkString
      // \u001b7 save cursor+attrs, \u001b8 restore.
      s"\u001b7$clears$header$entries\u001b8"

  def run(args: List[String]): IO[ExitCode] =
    val host = args.headOption
      .flatMap(Host.fromString)
      .orElse(sys.env.get("CHAT_SERVER_HOST").flatMap(Host.fromString))
      .getOrElse(defaultHost)

    val port = args
      .lift(1)
      .flatMap(Port.fromString)
      .orElse(sys.env.get("CHAT_SERVER_PORT").flatMap(Port.fromString))
      .getOrElse(defaultPort)

    for
      _ <- logger.info(s"Mugge Chat Client starting...")
      _ <- logger.info(s"Server: $host:$port")
      myUsername <- getUsername
      githubUsername <- Authentication.detectGithubUsername().flatMap(IO.fromEither)
      _ <- logger.info(s"The github username is: ${githubUsername}")
      _ <- githubUsername match
        case Some(ghu) => logger.debug(s"Detected GitHub username: $ghu")
        case None      => logger.error("Could not detect GitHub username from git config")

      // Trust-all: the server presents an ephemeral self-signed cert, so there
      // is nothing to validate — this only encrypts the transport.
      tlsContext <- Network[IO].tlsContext.insecure
      exitCode <- Network[IO]
        .client(SocketAddress(host, port))
        .use { rawSocket =>
          tlsContext.client(rawSocket).use { socket =>
            for
              _ <- IO.println(s"Connected to chat server at $host:$port")
              state <- Ref.of[IO, ClientState](
                ClientState(
                  githubUsername = githubUsername
                )
              )
              code <- handleConnection(socket, myUsername, state)
            yield code
          }
        }
        .handleErrorWith {
          case _: IOException =>
            logger.error(s"Failed to connect to server at $host:$port") *> IO(ExitCode.Error)
          case err =>
            logger.error(s"Error: ${err.getMessage}") *> IO(ExitCode.Error)
        }
    yield exitCode

  private def getHostname: IO[String] =
    sys.env.get("MUGGE_HOSTNAME") match
      case Some(hostname) => IO.pure(hostname)
      case None =>
        IO.blocking(java.net.InetAddress.getLocalHost.getHostName)
          .handleError(_ => "unknown-client")

  private def getUsername: IO[String] =
    getHostname.map(UserMapping.mapHostname)

  private def handleConnection(
      socket: Socket[IO],
      myUsername: String,
      state: Ref[IO, ClientState]
  ): IO[ExitCode] = {
    val initialDataIO: IO[String] = for {
      hostname <- getHostname
      currentState <- state.get
      privateKey <- currentState.githubUsername.flatTraverse { _ =>
        Authentication
          .loadPrivateKey()
          .flatMap {
            case Right(key) => IO.pure(Some(key))
            case Left(err) =>
              logger.warn(s"Could not load SSH private key: ${err.message}").as(None)
          }
          .handleErrorWith { err =>
            logger.error(s"Could not load SSH private key: ${err.getMessage}").as(None)
          }
      }
      _ <- privateKey.traverse_(key => state.update(_.copy(privateKey = Some(key))))
      authData = currentState.githubUsername
        .filter(_ => privateKey.isDefined)
        .map(ghu => s"auto-auth:$ghu")
        .getOrElse("")
      finalString = List(hostname, s"proto:$protocolVersion", authData)
        .filter(_.nonEmpty)
        .mkString("\n") + "\n\n"
      _ <- logger.debug(s"Prepared initial data to send.")
    } yield finalString

    (
      detectTerminal,
      // Fixed for the process: is our stdout backed by a terminal device at
      // all? (Under the dtach service this stays true even while headless.)
      IO(System.console() != null),
      Mutex[IO],
      // Bounded to keep a stuck socket from ballooning memory; offers apply
      // backpressure (the file-send loop naturally throttles to the socket).
      Queue.bounded[IO, String](1024),
      Deferred[IO, Either[Throwable, Unit]],
      IO.monotonic.flatMap(t => Ref.of[IO, FiniteDuration](t)),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, String](""),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, Int](0),
      Ref.of[IO, Option[(Int, Int)]](None)
    ).tupled.flatMap { tup =>
      val (
        initialSize,
        pty,
        mutex,
        outgoingQueue,
        halt,
        lastReceived,
        connectionLost,
        incompatible,
        input,
        composing,
        blockLines,
        termSize
      ) = tup
      val ui = new Ui(mutex, state, input, blockLines, pty, termSize)

      val serverWriter: Stream[IO, Nothing] =
        (Stream.eval(initialDataIO) ++ Stream.fromQueueUnterminated(outgoingQueue))
          .map(_ + "\n")
          .evalTap {
            case data if controlNoise(data.trim) => IO.unit
            // Never write secrets or file bytes to the log verbatim.
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
          myUsername,
          state,
          outgoingQueue,
          ui,
          lastReceived,
          halt,
          incompatible
        )
          .onFinalize(logger.debug("Server reader stream finished."))

      val userReader: Stream[IO, Unit] =
        readFromUser(outgoingQueue, halt, state, ui, input, composing)
          .onFinalize(logger.debug("User reader stream finished."))

      val pinger: Stream[IO, Unit] =
        Stream
          .awakeEvery[IO](pingInterval)
          .evalMap(_ => outgoingQueue.offer("PING"))

      // Re-assert TYPING while the user is still composing so the server's
      // typing lease never lapses mid-word; membership-stable so it triggers
      // no rebroadcast storm on the server side.
      val typingRefresher: Stream[IO, Unit] =
        Stream.awakeEvery[IO](typingRefreshInterval).evalMap { _ =>
          (composing.get, input.get).flatMapN { (c, inp) =>
            if c && inp.nonEmpty then outgoingQueue.offer("TYPING") else IO.unit
          }
        }

      // Trips when no line (chat, PONG, anything) has arrived for deadAfter,
      // turning a silent Azure drop into a clean teardown + nonzero exit so a
      // supervisor (task 8) restarts us instead of us looking alive but deaf.
      val watchdog: Stream[IO, Unit] =
        Stream.awakeEvery[IO](watchdogInterval).evalMap { _ =>
          for
            now <- IO.monotonic
            last <- lastReceived.get
            _ <-
              if (now - last) > deadAfter then
                connectionLost.set(true) *>
                  ui.printLine("Connection lost: no response from server.") *>
                  halt.complete(Right(())).void
              else IO.unit
          yield ()
        }

      // Window size isn't fixed: under the dtach-based service the pty starts
      // headless (0x0) and only gains real dimensions when someone runs `mugge`
      // to attach. Poll for that transition so the online-users panel and inline
      // input rendering come to life on attach (and fold back to plain output on
      // detach). Pointless without a pty in the first place.
      val terminalWatcher: Stream[IO, Unit] =
        if !pty then Stream.empty.covary[IO]
        else
          Stream.awakeEvery[IO](1.second).evalMap { _ =>
            detectTerminal.flatMap { latest =>
              termSize.getAndSet(latest).flatMap { previous =>
                if previous == latest then IO.unit else ui.onResize
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
          terminalWatcher
        )

      rawMode(pty).use { _ =>
        termSize.set(initialSize) >>
          logger.debug("Starting chat streams...") >>
          ui.refreshInput >>
          streams.parJoinUnbounded
            .interruptWhen(halt)
            .compile
            .drain
            .handleErrorWith { err =>
              connectionLost.set(true) *>
                logger
                  .error(s"\nConnection error: ${Option(err.getMessage).getOrElse(err.toString)}")
            } >>
          (connectionLost.get, incompatible.get).flatMapN { (lost, incompat) =>
            if incompat then IO.pure(ExitCode.Error) // server already printed guidance
            else if lost then
              IO.println("Connection lost — restart the client to reconnect.").as(ExitCode.Error)
            else IO.println("Bye!").as(ExitCode.Success)
          }
      }
    }
  }

  // Names of control lines that must never reach the client log.
  private def controlNoise(line: String): Boolean =
    line == "PING" || line == "TYPING" || line == "TYPINGSTOP"

  // Outbound lines whose contents must never be logged verbatim: auth
  // signatures (challenge-response secret material) and file payloads.
  private def sensitiveOutbound(line: String): Boolean =
    line.startsWith("SIGNATURE:") || line.startsWith("/verify ") ||
      line.startsWith("FILEDATA:") || line.startsWith("FILEEND:")

  // Puts the controlling terminal into non-canonical, no-echo mode so we can
  // observe keystrokes as they happen (for typing indicators) and render the
  // input line ourselves. Original settings are captured up front and restored
  // on release, even on crash/cancel. A no-op when we have no TTY.
  private def rawMode(pty: Boolean): Resource[IO, Unit] =
    if !pty then Resource.unit[IO]
    else
      Resource
        .make(
          IO.blocking(Seq("sh", "-c", "stty -g < /dev/tty").!!.trim)
            .flatTap(_ =>
              IO.blocking(Seq("sh", "-c", "stty -icanon -echo min 1 time 0 < /dev/tty").!).void
            )
            .handleError(_ => "")
        )(saved =>
          if saved.isEmpty then IO.unit
          else IO.blocking(Seq("sh", "-c", s"stty $saved < /dev/tty").!).attempt.void
        )
        .void

  private def readFromServer(
      socket: Socket[IO],
      myUsername: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui,
      lastReceived: Ref[IO, FiniteDuration],
      halt: Deferred[IO, Either[Throwable, Unit]],
      incompatible: Ref[IO, Boolean]
  ): Stream[IO, Nothing] =
    socket.reads
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .evalMap { msg =>
        IO.monotonic.flatMap(lastReceived.set) *> {
          if msg == "PONG" then IO.unit
          else if msg.startsWith("INCOMPATIBLE:") then
            // Server refused us on version grounds; it already includes the
            // update-and-rebuild guidance. Show it and tear down cleanly.
            incompatible.set(true) *>
              ui.printLine(msg.drop("INCOMPATIBLE:".length)) *>
              halt.complete(Right(())).void
          else if msg.startsWith("CHALLENGE:") then
            handleAutoChallenge(msg.drop(10), state, outgoingQueue)
          else if msg.startsWith("Challenge: ") then
            handleManualChallenge(msg.drop(11), state, outgoingQueue)
          else if msg.startsWith("USERS:") then
            val users = msg.drop(6).split(",").map(_.trim).filter(_.nonEmpty).toList
            ui.setUsers(users)
          else if msg.startsWith("TYPING:") then
            // Belt-and-braces: the server already excludes us, but drop our own
            // name too so we never see ourselves listed as typing.
            val users = msg
              .drop(7)
              .split(",")
              .map(_.trim)
              .filter(_.nonEmpty)
              .filterNot(_.equalsIgnoreCase(myUsername))
              .toList
            ui.setTyping(users)
          else if msg.startsWith("FILEOFFER:") then handleFileOffer(msg, state, ui)
          else if msg.startsWith("FILEACCEPT:") then
            handleFileAccept(msg.drop(11).trim, state, outgoingQueue, ui)
          else if msg.startsWith("FILEREJECT:") then handleFileReject(msg.drop(11).trim, state, ui)
          else if msg.startsWith("FILEDATA:") then handleFileData(msg, state)
          else if msg.startsWith("FILEEND:") then handleFileEnd(msg, state, ui)
          else if isFromServer(msg) && msg.contains("Authentication successful!") then
            state.update(_.copy(authenticated = true)) >>
              ui.printLine(msg) >>
              ui.printLine("You can now start chatting!")
          else
            colorizeForDisplay(msg, state).flatMap(ui.printLine) >>
              checkForMentions(msg, myUsername) >>
              checkForPings(msg, myUsername, state)
        }
      }
      .drain

  private def handleAutoChallenge(
      challenge: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String]
  ): IO[Unit] =
    for {
      currentState <- state.get
      _ <- (currentState.privateKey, currentState.githubUsername) match {
        case (Some(privateKey), Some(githubUsername)) =>
          for {
            _ <- logger.debug(s"Received auto-auth challenge, signing for '$githubUsername'...")
            signature <- Authentication.signChallenge(challenge, privateKey).flatMap(IO.fromEither)
            _ <- outgoingQueue.offer(s"SIGNATURE:$signature")
            _ <- logger.debug("Auto-authentication response sent to queue.")
          } yield ()

        case _ =>
          logger.debug("Cannot auto-authenticate: missing private key or GitHub username.")
      }
    } yield ()

  private def handleManualChallenge(
      challenge: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String]
  ): IO[Unit] =
    for
      currentState <- state.get
      _ <- currentState.privateKey match
        case Some(privateKey) =>
          for
            _ <- logger.debug(s"Received authentication challenge, auto-signing...")
            signature <- Authentication.signChallenge(challenge, privateKey).flatMap(IO.fromEither)
            _ <- outgoingQueue.offer(s"/verify $signature")
          yield ()
        case None =>
          logger.error("Cannot sign challenge: SSH private key not loaded")
    yield ()

  private def readFromUser(
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      input: Ref[IO, String],
      composing: Ref[IO, Boolean]
  ): Stream[IO, Nothing] =
    if !ui.isTty then
      // No TTY (piped/redirected): stay line-based, no typing signals, no
      // escapes — the terminal still echoes and there is nothing to erase.
      Stream
        .repeatEval(Console[IO].readLine)
        .evalMap {
          case s if s == null         => halt.complete(Right(())).void
          case "/quit" if serviceMode => ui.printLine(quitHint)
          case "/quit"                => halt.complete(Right(())).void
          case s if s.startsWith("/sendfile ") =>
            prepareSendFile(s.drop(10), state, outgoingQueue, ui)
          case s => outgoingQueue.offer(s)
        }
        .drain
    else
      // Raw mode: assemble lines from single characters ourselves so we can
      // observe typing as it happens and render the input line under our
      // control (echo is off).
      fs2.io
        .stdin[IO](64)
        .through(text.utf8.decode)
        .flatMap(chunk => Stream.emits(chunk.toList))
        .evalMap(ch => handleInputChar(ch, outgoingQueue, halt, state, ui, input, composing))
        .drain

  private def handleInputChar(
      ch: Char,
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      input: Ref[IO, String],
      composing: Ref[IO, Boolean]
  ): IO[Unit] =
    ch match
      case '\n' | '\r' =>
        input.getAndSet("").flatMap { line =>
          ui.refreshInput *>
            stopTyping(outgoingQueue, composing) *>
            dispatchLine(line.trim, outgoingQueue, halt, state, ui)
        }
      case '\u007f' | '\b' =>
        input.updateAndGet(s => if s.isEmpty then s else s.dropRight(1)).flatMap { s =>
          ui.refreshInput *>
            (if s.isEmpty then stopTyping(outgoingQueue, composing) else IO.unit)
        }
      case '\u0004' => // Ctrl-D on an empty prompt behaves like /quit
        if serviceMode then ui.printLine(quitHint) else halt.complete(Right(())).void
      case c if c >= ' ' =>
        input.update(_ + c) *>
          ui.refreshInput *>
          startTyping(outgoingQueue, composing)
      case _ => IO.unit

  private def dispatchLine(
      line: String,
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui
  ): IO[Unit] =
    if line.isEmpty then IO.unit
    else if line == "/quit" then
      if serviceMode then ui.printLine(quitHint) else halt.complete(Right(())).void
    else if line.startsWith("/sendfile ") then
      prepareSendFile(line.drop(10), state, outgoingQueue, ui)
    else outgoingQueue.offer(line)

  private def startTyping(
      outgoingQueue: Queue[IO, String],
      composing: Ref[IO, Boolean]
  ): IO[Unit] =
    composing.getAndSet(true).flatMap(was => if was then IO.unit else outgoingQueue.offer("TYPING"))

  private def stopTyping(
      outgoingQueue: Queue[IO, String],
      composing: Ref[IO, Boolean]
  ): IO[Unit] =
    composing
      .getAndSet(false)
      .flatMap(was => if was then outgoingQueue.offer("TYPINGSTOP") else IO.unit)

  private def checkForMentions(line: String, myUsername: String): IO[Unit] =
    val messagePattern = """^\[(\d{2}:\d{2}:\d{2})\] [✓?] ([^:]+): (.+)$""".r
    val mentionPattern = s"@(\\w+)".r

    line match
      case messagePattern(time, sender, content) =>
        if !content.startsWith("!ping") then
          val mentions = mentionPattern.findAllMatchIn(content).map(_.group(1)).toSet

          if mentions.exists(_.equalsIgnoreCase(myUsername)) then
            sendNotification(
              title = s"Chat: $sender mentioned you",
              body = content,
              urgency = "normal"
            )
          else IO.unit
        else IO.unit
      case _ =>
        IO.unit

  // Per-sender ping allowance: at most maxPingsPerWindow notifications from a
  // given sender within pingWindow, after which that sender is muted until the
  // window rolls off. Stops a `!ping @me 1000` from flooding the tray.
  private val maxPingsPerWindow = 3
  private val pingWindow = 5.minutes

  private def checkForPings(
      line: String,
      myUsername: String,
      state: Ref[IO, ClientState]
  ): IO[Unit] =
    val messagePattern = """^\[(\d{2}:\d{2}:\d{2})\] [✓?] ([^:]+): (.+)$""".r
    val pingPattern = """^!ping\s+@(\w+)(?:\s+(\d+))?""".r

    line match
      case messagePattern(time, sender, content) =>
        content match
          case pingPattern(targetUser, countStr) if targetUser.equalsIgnoreCase(myUsername) =>
            val requested = Option(countStr).flatMap(_.toIntOption).getOrElse(1).max(1)
            IO.monotonic.flatMap { now =>
              state
                .modify { st =>
                  val recent = st.pingHistory
                    .getOrElse(sender, Nil)
                    .filter(t => now - t < pingWindow)
                  val allowed = (maxPingsPerWindow - recent.size).max(0).min(requested)
                  val updated = recent ++ List.fill(allowed)(now)
                  (st.copy(pingHistory = st.pingHistory.updated(sender, updated)), allowed)
                }
                .flatMap { allowed =>
                  if allowed <= 0 then IO.unit
                  else sendMultiplePings(sender, time, allowed)
                }
            }
          case _ => IO.unit
      case _ =>
        IO.unit

  private def sendMultiplePings(sender: String, time: String, count: Int): IO[Unit] =
    val delay = 500.milliseconds

    (1 to count).toList.traverse_ { i =>
      sendNotification(
        title = s"🔔 Mention from $sender (${i}/$count)",
        body = s"$sender mentioned you at $time",
        urgency = "critical",
        timeout = 0
      ) >> IO.sleep(delay)
    }

  private def sendNotification(
      title: String,
      body: String,
      urgency: String = "normal",
      timeout: Int = 5000
  ): IO[Unit] = {
    val command = Seq(
      "notify-send",
      "-u",
      urgency,
      "-i",
      "dialog-information",
      "-a",
      "Terminal Chat",
      "-t",
      timeout.toString,
      title,
      body
    )

    IO.blocking(command.!).void.handleErrorWith { e =>
      logger.error(s"[Notification Error] ${e.getMessage}") *>
        logger.info(s"[Notification] $title: $body")
    }
  }

  // Sender: user typed `/sendfile @user <path>`. Validate locally, then send
  // the wire form `/sendfile @user <id> <size> <name>` (server can't stat the
  // file), remembering id -> path until the receiver accepts.
  private def prepareSendFile(
      rest: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): IO[Unit] =
    rest.trim.split(" ", 2) match
      case Array(targetToken, rawPath) if targetToken.startsWith("@") =>
        val path = Paths.get(rawPath.trim)
        IO.blocking {
          val regular = Files.isRegularFile(path)
          val readable = regular && Files.isReadable(path)
          val size = if regular then Files.size(path) else -1L
          (Files.exists(path), regular, readable, size)
        }.attempt
          .flatMap {
            case Right((false, _, _, _)) => ui.printLine(s"File not found: $rawPath")
            case Right((_, false, _, _)) => ui.printLine(s"Not a regular file: $rawPath")
            case Right((_, _, false, _)) => ui.printLine(s"File is not readable: $rawPath")
            case Right((_, _, _, s)) if s > maxFileSize =>
              ui.printLine(s"File too large ($s bytes). Max is $maxFileSize bytes.")
            case Right((_, _, _, size)) =>
              val id = java.util.UUID.randomUUID().toString.take(8)
              val name = path.getFileName.toString
              state.update(st =>
                st.copy(outgoingFiles = st.outgoingFiles + (id -> OutgoingFile(path, name, size)))
              ) *>
                outgoingQueue.offer(s"/sendfile $targetToken $id $size $name")
            case Left(err) => ui.printLine(s"Could not read file: ${err.getMessage}")
          }
      case _ =>
        ui.printLine("Usage: /sendfile @user <path>")

  private def handleFileOffer(
      msg: String,
      state: Ref[IO, ClientState],
      ui: Ui
  ): IO[Unit] =
    msg.split(":", 5) match
      case Array(_, id, from, sizeStr, name) =>
        val size = sizeStr.toLongOption.getOrElse(0L)
        val safe = sanitizeFilename(name)
        state.update(st =>
          st.copy(incomingFiles = st.incomingFiles + (id -> IncomingFile(from, safe, size)))
        ) *>
          ui.printLine(
            s"$from wants to send \"$safe\" ($size bytes). " +
              s"Accept with /acceptfile $id or decline with /rejectfile $id"
          ) *>
          sendNotification(
            title = s"📎 File offer from $from",
            body = s"$safe ($size bytes) — /acceptfile $id or /rejectfile $id",
            urgency = "critical",
            timeout = 0
          )
      case _ => IO.unit

  // Run detached (via `.start`) so the server read loop keeps flowing while sending.
  private def handleFileAccept(
      id: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): IO[Unit] =
    state
      .modify(st => (st.copy(outgoingFiles = st.outgoingFiles - id), st.outgoingFiles.get(id)))
      .flatMap {
        case Some(out) =>
          ui.printLine(s"Offer accepted; sending ${out.name}...") *>
            sendFileData(id, out, outgoingQueue, ui).start.void
        case None => IO.unit
      }

  // Stream the file from disk in fixed-size chunks (bounded memory even for a
  // 10 MiB file), base64-ing each and folding a running SHA-256, rather than
  // reading the whole file into memory. Backpressure comes from the bounded
  // outgoing queue.
  private def sendFileData(
      id: String,
      out: OutgoingFile,
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): IO[Unit] =
    IO(MessageDigest.getInstance("SHA-256")).flatMap { md =>
      Fs2Files[IO]
        .readAll(Fs2Path.fromNioPath(out.path))
        .chunkN(fileChunkSize)
        .zipWithIndex
        .evalMap { case (chunk, seq) =>
          val bytes = chunk.toArray
          IO(md.update(bytes)) *>
            outgoingQueue.offer(s"FILEDATA:$id:$seq:${Base64.getEncoder.encodeToString(bytes)}")
        }
        .compile
        .drain
        .flatMap(_ => outgoingQueue.offer(s"FILEEND:$id:${toHex(md.digest())}"))
        .flatMap(_ => ui.printLine(s"Sent ${out.name} (${out.size} bytes)."))
        .handleErrorWith(err => ui.printLine(s"Failed to send ${out.name}: ${err.getMessage}"))
    }

  private def handleFileReject(
      id: String,
      state: Ref[IO, ClientState],
      ui: Ui
  ): IO[Unit] =
    state
      .modify(st => (st.copy(outgoingFiles = st.outgoingFiles - id), st.outgoingFiles.get(id)))
      .flatMap {
        case Some(out) => ui.printLine(s"${out.name} was rejected by the recipient.")
        case None      => IO.unit
      }

  private def handleFileData(msg: String, state: Ref[IO, ClientState]): IO[Unit] =
    msg.split(":", 4) match
      case Array(_, id, _, b64) =>
        state.get.map(_.incomingFiles.get(id)).flatMap {
          case None => IO.unit
          case Some(incoming) =>
            val bytes = Base64.getDecoder.decode(b64)
            incoming.temp match
              case Some(tmp) =>
                IO.blocking(Files.write(tmp, bytes, StandardOpenOption.APPEND)).void
              case None =>
                for
                  tmp <- IO.blocking(Files.createTempFile("mugge-", ".part"))
                  _ <- IO.blocking(Files.write(tmp, bytes, StandardOpenOption.APPEND))
                  _ <- state.update(st =>
                    st.copy(incomingFiles =
                      st.incomingFiles.updatedWith(id)(_.map(_.copy(temp = Some(tmp))))
                    )
                  )
                yield ()
        }
      case _ => IO.unit

  private def handleFileEnd(msg: String, state: Ref[IO, ClientState], ui: Ui): IO[Unit] =
    msg.split(":", 3) match
      case Array(_, id, sha) =>
        state
          .modify(st => (st.copy(incomingFiles = st.incomingFiles - id), st.incomingFiles.get(id)))
          .flatMap {
            case None => IO.unit
            case Some(incoming) =>
              for
                tmp <- incoming.temp match
                  case Some(t) => IO.pure(t)
                  case None    => IO.blocking(Files.createTempFile("mugge-", ".part"))
                _ <- finalizeIncoming(tmp, sha, incoming, ui)
              yield ()
          }
      case _ => IO.unit

  private def finalizeIncoming(
      tmp: Path,
      expectedSha: String,
      incoming: IncomingFile,
      ui: Ui
  ): IO[Unit] =
    streamSha256(tmp).flatMap { actualSha =>
      if !actualSha.equalsIgnoreCase(expectedSha) then
        IO.blocking(Files.deleteIfExists(tmp)).attempt.void *>
          ui.printLine(s"Checksum mismatch for ${incoming.name}; download discarded.")
      else
        for
          dir <- downloadDir
          _ <- IO.blocking(Files.createDirectories(dir))
          target <- IO.blocking(uniqueTarget(dir, incoming.name))
          _ <- IO.blocking(Files.move(tmp, target))
          _ <- ui.printLine(s"Saved ${incoming.name} from ${incoming.from} to $target")
          _ <- sendNotification(
            title = s"📎 File received from ${incoming.from}",
            body = s"Saved to $target",
            urgency = "normal"
          )
        yield ()
    }

  private def toHex(bytes: Array[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  // Fold a SHA-256 over the file on disk in chunks, without loading it wholesale.
  private def streamSha256(path: Path): IO[String] =
    IO(MessageDigest.getInstance("SHA-256")).flatMap { md =>
      Fs2Files[IO]
        .readAll(Fs2Path.fromNioPath(path))
        .chunks
        .evalMap(c => IO(md.update(c.toArray)))
        .compile
        .drain
        .map(_ => toHex(md.digest()))
    }

  private def sanitizeFilename(name: String): String =
    val base = name.replace("\\", "/").split("/").filter(_.nonEmpty).lastOption.getOrElse("file")
    val cleaned = base.trim
    if cleaned.isEmpty || cleaned == "." || cleaned == ".." then "file" else cleaned

  private def downloadDir: IO[Path] =
    IO {
      sys.env
        .get("XDG_DOWNLOAD_DIR")
        .filter(_.nonEmpty)
        .map(Paths.get(_))
        .getOrElse(Paths.get(System.getProperty("user.home"), "Downloads"))
    }

  private def uniqueTarget(dir: Path, name: String): Path =
    val initial = dir.resolve(name)
    if !Files.exists(initial) then initial
    else
      val dot = name.lastIndexOf('.')
      val (base, ext) =
        if dot > 0 then (name.substring(0, dot), name.substring(dot)) else (name, "")
      Iterator
        .from(1)
        .map(i => dir.resolve(s"$base ($i)$ext"))
        .find(p => !Files.exists(p))
        .get
