package chat

import cats.effect.*
import cats.effect.std.{Console, Mutex, Queue}
import cats.syntax.all.*
import fs2.*
import fs2.io.net.*
import com.comcast.ip4s.*
import java.io.IOException
import scala.sys.process.*
import scala.concurrent.duration.*
import java.security.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger => TLogger}

object ChatClient extends IOApp:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  private val defaultPort = port"5555"
  private val defaultHost = host"localhost"

  case class ClientState(
      authenticated: Boolean = false,
      githubUsername: Option[String] = None,
      privateKey: Option[PrivateKey] = None,
      colors: Map[String, Int] = Map.empty,
      onlineUsers: List[String] = Nil
  )

  private val ansiReset = "\u001b[0m"

  // Distinct 256-color foregrounds; assigned per sender in order of first
  // appearance so concurrent chatters stay visually separated.
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

  // Darker twin of each entry in ansiPalette (same index), used for the
  // message body so a sender's text reads as a weaker shade of their name.
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
              (cols.toIntOption, rows.toIntOption).tupled
            case _ => None
        }
        .handleError(_ => None)

  // Serializes all terminal writes so chat lines and the right-hand online-users
  // panel never interleave mid-escape. Without a TTY it degrades to plain lines.
  final class Ui(
      mutex: Mutex[IO],
      state: Ref[IO, ClientState],
      term: Option[(Int, Int)]
  ):
    def printLine(line: String): IO[Unit] =
      mutex.lock.surround {
        term match
          case None => Console[IO].println(line)
          case Some((cols, rows)) =>
            Console[IO].print(s"\r$line\n") *> redraw(cols, rows)
      }

    def setUsers(users: List[String]): IO[Unit] =
      state.update(_.copy(onlineUsers = users)) *>
        mutex.lock.surround {
          term match
            case None               => Console[IO].println(s"Online: ${users.mkString(", ")}")
            case Some((cols, rows)) => redraw(cols, rows)
        }

    private def redraw(cols: Int, rows: Int): IO[Unit] =
      for
        st <- state.get
        startCol = math.max(1, cols - panelWidth + 1)
        clearRows = math.min(rows - 1, 30)
        visible = st.onlineUsers.take(math.max(0, clearRows - 1))
        colored <- visible.traverse(u => colorIndexFor(u, state).map(idx => (u, ansiPalette(idx))))
        sb = new StringBuilder
        _ = sb.append("\u001b7") // save cursor + attrs
        _ = (1 to clearRows).foreach { r =>
          sb.append(s"\u001b[$r;${startCol}H")
          sb.append(" " * panelWidth)
        }
        _ = sb.append(
          s"\u001b[1;${startCol}H\u001b[1m\u2524 Online (${st.onlineUsers.size})\u001b[0m"
        )
        _ = colored.zipWithIndex.foreach { case ((u, color), i) =>
          val name = if u.length > panelWidth - 2 then u.take(panelWidth - 2) else u
          sb.append(s"\u001b[${2 + i};${startCol}H$color\u2022 $name$ansiReset")
        }
        _ = sb.append("\u001b8") // restore cursor
        _ <- Console[IO].print(sb.toString)
      yield ()

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

      exitCode <- Network[IO]
        .client(SocketAddress(host, port))
        .use { socket =>
          for
            _ <- IO.println(s"Connected to chat server at $host:$port")
            state <- Ref.of[IO, ClientState](
              ClientState(
                githubUsername = githubUsername
              )
            )
            _ <- handleConnection(socket, myUsername, state)
          yield ExitCode.Success
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
  ): IO[Unit] = {
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
      finalString = List(hostname, authData).filter(_.nonEmpty).mkString("\n") + "\n\n"
      _ <- logger.info(s"Prepared initial data to send.")
    } yield finalString

    (
      detectTerminal,
      Mutex[IO],
      Queue.unbounded[IO, String],
      Deferred[IO, Either[Throwable, Unit]]
    ).tupled.flatMap { case (term, mutex, outgoingQueue, halt) =>
      val ui = new Ui(mutex, state, term)

      val serverWriter: Stream[IO, Nothing] =
        (Stream.eval(initialDataIO) ++ Stream.fromQueueUnterminated(outgoingQueue))
          .map(_ + "\n")
          .evalTap(data => logger.info(s"Writing to server: $data"))
          .through(text.utf8.encode)
          .through(socket.writes)
          .onFinalize(logger.info("Server writer stream finished."))

      val serverReader: Stream[IO, Unit] =
        readFromServer(socket, myUsername, state, outgoingQueue, ui)
          .onFinalize(logger.info("Server reader stream finished."))

      val userReader: Stream[IO, Unit] =
        readFromUser(outgoingQueue, halt)
          .onFinalize(logger.info("User reader stream finished."))

      logger.info("Starting chat streams...") >>
        Stream(serverReader, serverWriter, userReader).parJoinUnbounded
          .interruptWhen(halt)
          .compile
          .drain
          .handleErrorWith { err =>
            logger.error(s"\nConnection error: ${Option(err.getMessage).getOrElse(err.toString)}")
          } >>
        IO.println("Bye!")
    }
  }

  private def readFromServer(
      socket: Socket[IO],
      myUsername: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): Stream[IO, Nothing] =
    socket.reads
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .evalMap { msg =>
        if msg.startsWith("CHALLENGE:") then handleAutoChallenge(msg.drop(10), state, outgoingQueue)
        else if msg.startsWith("Challenge: ") then
          handleManualChallenge(msg.drop(11), state, outgoingQueue)
        else if msg.startsWith("USERS:") then
          val users = msg.drop(6).split(",").map(_.trim).filter(_.nonEmpty).toList
          ui.setUsers(users)
        else if msg.contains("Authentication successful!") then
          state.update(_.copy(authenticated = true)) >>
            ui.printLine(msg) >>
            ui.printLine("You can now start chatting!")
        else
          colorizeForDisplay(msg, state).flatMap(ui.printLine) >>
            checkForMentions(msg, myUsername) >>
            checkForPings(msg, myUsername)
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
      halt: Deferred[IO, Either[Throwable, Unit]]
  ): Stream[IO, Nothing] =
    Stream
      .repeatEval(Console[IO].readLine)
      .evalMap {
        case s if s == null || s == "/quit" => halt.complete(Right(())).void
        case s                              => outgoingQueue.offer(s)
      }
      .drain

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

  private def checkForPings(line: String, myUsername: String): IO[Unit] =
    val messagePattern = """^\[(\d{2}:\d{2}:\d{2})\] [✓?] ([^:]+): (.+)$""".r
    val pingPattern = """^!ping\s+@(\w+)(?:\s+(\d+))?""".r

    line match
      case messagePattern(time, sender, content) =>
        content match
          case pingPattern(targetUser, countStr) =>
            if targetUser.equalsIgnoreCase(myUsername) then
              val count = Option(countStr).flatMap(_.toIntOption).getOrElse(1)
              val limitedCount = count.min(10).max(1)

              sendMultiplePings(sender, time, limitedCount)
            else IO.unit
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
