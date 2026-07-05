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
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.Base64
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger => TLogger}

object ChatClient extends IOApp:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  private val defaultPort = port"5555"
  private val defaultHost = host"localhost"

  private val maxFileSize = 10L * 1024 * 1024 // 10 MiB
  private val fileChunkSize = 48 * 1024 // raw bytes per chunk before base64

  // Application-level heartbeat: keeps the TCP flow non-idle so Azure's load
  // balancer (~4 min idle drop) never silently kills a quiet connection, and
  // doubles as a server-process liveness probe. deadAfter spans ~3 PONGs.
  private val pingInterval = 60.seconds
  private val watchdogInterval = 30.seconds
  private val deadAfter = 3.minutes

  // Sender-side: file the local user offered, kept until FILEACCEPT/FILEREJECT.
  case class OutgoingFile(path: Path, name: String, size: Long)

  // Receiver-side: an accepted offer being streamed in; temp is created lazily
  // on the first FILEDATA chunk and finalized on FILEEND.
  case class IncomingFile(from: String, name: String, size: Long, temp: Option[Path] = None)

  case class ClientState(
      authenticated: Boolean = false,
      githubUsername: Option[String] = None,
      privateKey: Option[PrivateKey] = None,
      colors: Map[String, Int] = Map.empty,
      onlineUsers: List[String] = Nil,
      outgoingFiles: Map[String, OutgoingFile] = Map.empty,
      incomingFiles: Map[String, IncomingFile] = Map.empty
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
            code <- handleConnection(socket, myUsername, state)
          yield code
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
      finalString = List(hostname, authData).filter(_.nonEmpty).mkString("\n") + "\n\n"
      _ <- logger.info(s"Prepared initial data to send.")
    } yield finalString

    (
      detectTerminal,
      Mutex[IO],
      Queue.unbounded[IO, String],
      Deferred[IO, Either[Throwable, Unit]],
      IO.monotonic.flatMap(t => Ref.of[IO, FiniteDuration](t)),
      Ref.of[IO, Boolean](false)
    ).tupled.flatMap { case (term, mutex, outgoingQueue, halt, lastReceived, connectionLost) =>
      val ui = new Ui(mutex, state, term)

      val serverWriter: Stream[IO, Nothing] =
        (Stream.eval(initialDataIO) ++ Stream.fromQueueUnterminated(outgoingQueue))
          .map(_ + "\n")
          .evalTap {
            case data if data.trim == "PING" => IO.unit // heartbeat stays invisible client-side
            case data                        => logger.info(s"Writing to server: $data")
          }
          .through(text.utf8.encode)
          .through(socket.writes)
          .onFinalize(logger.info("Server writer stream finished."))

      val serverReader: Stream[IO, Unit] =
        readFromServer(socket, myUsername, state, outgoingQueue, ui, lastReceived)
          .onFinalize(logger.info("Server reader stream finished."))

      val userReader: Stream[IO, Unit] =
        readFromUser(outgoingQueue, halt, state, ui)
          .onFinalize(logger.info("User reader stream finished."))

      val pinger: Stream[IO, Unit] =
        Stream
          .awakeEvery[IO](pingInterval)
          .evalMap(_ => outgoingQueue.offer("PING"))

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

      logger.info("Starting chat streams...") >>
        Stream(serverReader, serverWriter, userReader, pinger, watchdog).parJoinUnbounded
          .interruptWhen(halt)
          .compile
          .drain
          .handleErrorWith { err =>
            connectionLost.set(true) *>
              logger.error(s"\nConnection error: ${Option(err.getMessage).getOrElse(err.toString)}")
          } >>
        connectionLost.get.flatMap { lost =>
          if lost then
            IO.println("Connection lost — restart the client to reconnect.").as(ExitCode.Error)
          else IO.println("Bye!").as(ExitCode.Success)
        }
    }
  }

  private def readFromServer(
      socket: Socket[IO],
      myUsername: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui,
      lastReceived: Ref[IO, FiniteDuration]
  ): Stream[IO, Nothing] =
    socket.reads
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .evalMap { msg =>
        IO.monotonic.flatMap(lastReceived.set) *> {
          if msg == "PONG" then IO.unit
          else if msg.startsWith("CHALLENGE:") then
            handleAutoChallenge(msg.drop(10), state, outgoingQueue)
          else if msg.startsWith("Challenge: ") then
            handleManualChallenge(msg.drop(11), state, outgoingQueue)
          else if msg.startsWith("USERS:") then
            val users = msg.drop(6).split(",").map(_.trim).filter(_.nonEmpty).toList
            ui.setUsers(users)
          else if msg.startsWith("FILEOFFER:") then handleFileOffer(msg, state, ui)
          else if msg.startsWith("FILEACCEPT:") then
            handleFileAccept(msg.drop(11).trim, state, outgoingQueue, ui)
          else if msg.startsWith("FILEREJECT:") then handleFileReject(msg.drop(11).trim, state, ui)
          else if msg.startsWith("FILEDATA:") then handleFileData(msg, state)
          else if msg.startsWith("FILEEND:") then handleFileEnd(msg, state, ui)
          else if msg.contains("Authentication successful!") then
            state.update(_.copy(authenticated = true)) >>
              ui.printLine(msg) >>
              ui.printLine("You can now start chatting!")
          else
            colorizeForDisplay(msg, state).flatMap(ui.printLine) >>
              checkForMentions(msg, myUsername) >>
              checkForPings(msg, myUsername)
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
      ui: Ui
  ): Stream[IO, Nothing] =
    Stream
      .repeatEval(Console[IO].readLine)
      .evalMap {
        case s if s == null || s == "/quit" => halt.complete(Right(())).void
        case s if s.startsWith("/sendfile ") =>
          prepareSendFile(s.drop(10), state, outgoingQueue, ui)
        case s => outgoingQueue.offer(s)
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

  // ---- File transfer -------------------------------------------------------

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

  // Receiver: an offer arrived. Remember its metadata and alert the user both
  // in the chat pane and with a desktop notification.
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

  // Sender: receiver approved; stream the file as base64 FILEDATA chunks then a
  // FILEEND with the sha256. Run detached so the read loop keeps flowing.
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

  private def sendFileData(
      id: String,
      out: OutgoingFile,
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): IO[Unit] =
    IO.blocking(Files.readAllBytes(out.path)).attempt.flatMap {
      case Left(err) =>
        ui.printLine(s"Failed to read ${out.name}: ${err.getMessage}")
      case Right(bytes) =>
        val sha = sha256Hex(bytes)
        val chunks = bytes.grouped(fileChunkSize).zipWithIndex.toList
        chunks.traverse_ { case (chunk, seq) =>
          outgoingQueue.offer(s"FILEDATA:$id:$seq:${Base64.getEncoder.encodeToString(chunk)}")
        } *>
          outgoingQueue.offer(s"FILEEND:$id:$sha") *>
          ui.printLine(s"Sent ${out.name} (${out.size} bytes).")
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

  // Receiver: append an incoming chunk to a lazily-created temp file.
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

  // Receiver: verify sha256 and move the temp file into the download dir.
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
    IO.blocking(Files.readAllBytes(tmp)).flatMap { bytes =>
      if !sha256Hex(bytes).equalsIgnoreCase(expectedSha) then
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

  private def sha256Hex(bytes: Array[Byte]): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .map(b => f"${b & 0xff}%02x")
      .mkString

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
