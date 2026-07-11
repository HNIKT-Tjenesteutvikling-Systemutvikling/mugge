package chat

import cats.effect.*
import cats.effect.std.{Console, Mutex, Queue, UUIDGen}
import cats.syntax.all.*
import cats.mtl.{Raise as MtlRaise}
import cats.mtl.Handle.allow
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
  private val protocolVersion = 4

  private val serviceMode: Boolean = sys.env.get("MUGGE_SERVICE").contains("1")

  private val quitHint =
    "/quit is disabled here — this chat runs in the background. Press Ctrl-\\ " +
      "(or just close the terminal) to leave without disconnecting. To stop it " +
      "entirely: systemctl --user stop mugge-chat"

  private val maxFileSize = 10L * 1024 * 1024
  private val fileChunkSize = 48 * 1024

  private val maxPasteLines = 100
  private val maxPasteChars = 8 * 1024
  private val pasteLineDelay = 400.milliseconds

  private val pingInterval = 60.seconds
  private val watchdogInterval = 30.seconds
  private val deadAfter = 3.minutes

  private val typingRefreshInterval = 3.seconds

  private val suspendGraceThreshold = 30.seconds

  private val reconnectInitialBackoff = 2.seconds
  private val reconnectMaxBackoff = 30.seconds

  case class OutgoingFile(path: Path, name: String, size: Long)

  case class IncomingFile(
      from: String,
      name: String,
      size: Long,
      temp: Option[Path] = None,
      received: Long = 0
  )

  private enum FileError:
    case NotFound(path: String)
    case NotRegular(path: String)
    case NotReadable(path: String)
    case TooLarge(size: Long)
    case Unreadable(detail: String)
    case ChecksumMismatch(name: String)

  private def fileErrorMessage(e: FileError): String = e match
    case FileError.NotFound(p)         => s"File not found: $p"
    case FileError.NotRegular(p)       => s"Not a regular file: $p"
    case FileError.NotReadable(p)      => s"File is not readable: $p"
    case FileError.TooLarge(s)         => s"File too large ($s bytes). Max is $maxFileSize bytes."
    case FileError.Unreadable(d)       => s"Could not read file: $d"
    case FileError.ChecksumMismatch(n) => s"Checksum mismatch for $n; download discarded."

  private def raiseFile[A](e: FileError)(using r: MtlRaise[IO, FileError]): IO[A] =
    r.raise(e)

  private def progressMilestone(prev: Long, now: Long, total: Long): Option[Int] =
    if total <= 0 then None
    else
      val p = (prev * 4 / total).toInt
      val n = (now * 4 / total).toInt
      if n > p && n < 4 then Some(n * 25) else None

  case class ClientState(
      authenticated: Boolean = false,
      githubUsername: Option[String] = None,
      privateKey: Option[PrivateKey] = None,
      colors: Map[String, Int] = Map.empty,
      onlineUsers: List[String] = Nil,
      typingUsers: List[String] = Nil,
      outgoingFiles: Map[String, OutgoingFile] = Map.empty,
      incomingFiles: Map[String, IncomingFile] = Map.empty,
      pingHistory: Map[String, List[FiniteDuration]] = Map.empty,
      inVoice: Boolean = false,
      muted: Boolean = false,
      voiceUsers: List[String] = Nil
  )

  private case class Voice(handle: Audio.Handle, teardown: IO[Unit])

  private enum SessionOutcome:
    case Quit, Incompatible, Lost, ConnectFailed

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

  private def wrapAnsi(s: String, width: Int): List[String] =
    val rows = List.newBuilder[String]
    val row = new StringBuilder
    var cells = 0
    var i = 0
    while i < s.length do
      if s.charAt(i) == '\u001b' then
        val start = i
        i += 1
        if i < s.length && s.charAt(i) == '[' then
          i += 1
          while i < s.length && (s.charAt(i) < '@' || s.charAt(i) > '~') do i += 1
          if i < s.length then i += 1
        else if i < s.length then i += 1
        row.append(s.substring(start, i))
      else
        if cells == width then
          rows += row.result()
          row.setLength(0)
          cells = 0
        val n = Character.charCount(s.codePointAt(i))
        row.append(s.substring(i, i + n))
        cells += 1
        i += n
    rows += row.result()
    rows.result()

  private val displayPattern =
    """^\[(\d{2}:\d{2}:\d{2})\] ([✓?]) ([^:]+): (.*)$""".r

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

  private def detectTerminal: IO[Option[(Int, Int)]] =
    IO(Option(System.console())).flatMap {
      case None => IO.pure(None)
      case Some(_) =>
        IO.blocking(Seq("sh", "-c", "stty size < /dev/tty").!!.trim)
          .map { out =>
            out.split("\\s+").toList match
              case rows :: cols :: Nil =>
                (cols.toIntOption, rows.toIntOption).tupled.filter((c, r) => c > 0 && r > 0)
              case _ => None
          }
          .handleError(_ => None)
    }

  private val inputPrompt = "> "

  private def formatTyping(users: List[String]): Option[String] =
    users match
      case Nil      => None
      case a :: Nil => Some(s"$a is typing...")
      case _ =>
        val init = users.init.mkString(", ")
        Some(s"$init and ${users.last} is typing...")

  private enum InputToken:
    case Ch(c: Char)
    case PasteStart, PasteEnd

  private enum EscState:
    case Ground, Esc
    case Csi(params: String)

  private def tokenize(chars: Stream[IO, Char]): Stream[IO, InputToken] =
    chars
      .mapAccumulate(EscState.Ground: EscState) { (st, c) =>
        st match
          case EscState.Ground =>
            if c == '\u001b' then (EscState.Esc, Nil)
            else (EscState.Ground, List(InputToken.Ch(c)))
          case EscState.Esc =>
            if c == '[' then (EscState.Csi(""), Nil) else (EscState.Ground, Nil)
          case EscState.Csi(params) =>
            if c >= '@' && c <= '~' then
              val tok = (params, c) match
                case ("200", '~') => List(InputToken.PasteStart)
                case ("201", '~') => List(InputToken.PasteEnd)
                case _            => Nil
              (EscState.Ground, tok)
            else (EscState.Csi(params + c), Nil)
      }
      .flatMap((_, toks) => Stream.emits(toks))

  final case class InputCtl(
      text: Ref[IO, String],
      hint: Ref[IO, Option[String]],
      pendingPaste: Ref[IO, Option[String]],
      paste: Ref[IO, Option[(List[Char], Int)]],
      composing: Ref[IO, Boolean]
  )

  final class Ui(
      mutex: Mutex[IO],
      state: Ref[IO, ClientState],
      ictl: InputCtl,
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

    def setVoiceUsers(users: List[String]): IO[Unit] =
      state.modify(st => (st.copy(voiceUsers = users), st.voiceUsers != users)).flatMap { changed =>
        if !changed then IO.unit
        else
          mutex.lock.surround {
            termSize.get.flatMap {
              case None               => IO.unit
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
        inp <- ictl.text.get
        paste <- ictl.pendingPaste.get
        hint <- ictl.hint.get
        prev <- blockLines.get
        startCol = math.max(1, cols - panelWidth + 1)
        textWidth = math.max(1, startCol - 1)
        clearRows = math.min(rows - 1, 30)
        visible = st.onlineUsers.take(math.max(0, clearRows - 1))
        colored <- visible.traverse(u => colorIndexFor(u, state).map(idx => (u, ansiPalette(idx))))
        (blockStr, newCount) = renderBlock(st, inp, paste, hint, textWidth, rows)
        sb = new StringBuilder
        _ = sb.append(eraseBlock(prev))
        _ = chat.foreach(l => wrapAnsi(l, textWidth).foreach(r => sb.append(r).append("\n")))
        _ = sb.append(blockStr)
        _ = sb.append(
          panelStr(colored, st.voiceUsers.toSet, st.onlineUsers.size, startCol, clearRows)
        )
        _ <- Console[IO].print(sb.toString)
        _ <- blockLines.set(newCount)
      yield ()

    private def eraseBlock(n: Int): String =
      if n <= 0 then ""
      else
        val up = if n > 1 then s"\u001b[${n - 1}A" else ""
        s"\r$up\u001b[0J"

    private def renderBlock(
        st: ClientState,
        inp: String,
        paste: Option[String],
        hint: Option[String],
        width: Int,
        rows: Int
    ): (String, Int) =
      def dim(s: String) = s"\u001b[2m${wrapAnsi(s, width).head}$ansiReset"
      val hintRow = hint.map(dim)
      val typingRow = formatTyping(st.typingUsers).map(dim)
      val pasteTag =
        paste.fold("")(p => s"\u001b[2m[paste: ${p.count(_ == '\n') + 1} lines]$ansiReset")
      // Tail-follow when the input is taller than the screen.
      val inputRows =
        wrapAnsi(inputPrompt + inp + pasteTag, width).takeRight(math.max(1, rows - 2))
      val all = hintRow.toList ++ typingRow.toList ++ inputRows
      (all.map(r => s"\r\u001b[2K$r").mkString("\n"), all.size)

    private def panelStr(
        colored: List[(String, String)],
        voice: Set[String],
        total: Int,
        startCol: Int,
        clearRows: Int
    ): String =
      val clears =
        (1 to clearRows).map(r => s"\u001b[$r;${startCol}H" + " " * panelWidth).mkString
      val header = s"\u001b[1;${startCol}H\u001b[1m\u2524 Online ($total)\u001b[0m"
      val entries = colored.zipWithIndex.map { case ((u, color), i) =>
        val bullet = if voice.contains(u) then "\u266a" else "\u2022"
        val name = if u.length > panelWidth - 2 then u.take(panelWidth - 2) else u
        s"\u001b[${2 + i};${startCol}H$color$bullet $name$ansiReset"
      }.mkString
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
      githubUsername <- allow[Authentication.AuthError] {
        Authentication.detectGithubUsername()
      }.rescue { err =>
        logger.warn(s"Could not detect GitHub username: ${err.message}").as(None)
      }
      _ <- logger.info(s"The github username is: ${githubUsername}")
      _ <- githubUsername match
        case Some(ghu) => logger.debug(s"Detected GitHub username: $ghu")
        case None      => logger.error("Could not detect GitHub username from git config")

      tlsContext <- Network[IO].tlsContext.insecure
      connectOnce = Network[IO]
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
              outcome <- handleConnection(socket, myUsername, state)
            yield outcome
          }
        }
        .handleErrorWith {
          case _: IOException =>
            logger
              .error(s"Failed to connect to server at $host:$port")
              .as(SessionOutcome.ConnectFailed)
          case err =>
            logger.error(s"Error: ${err.getMessage}").as(SessionOutcome.ConnectFailed)
        }
      exitCode <- reconnectLoop(connectOnce, reconnectInitialBackoff)
    yield exitCode

  private def reconnectLoop(
      connectOnce: IO[SessionOutcome],
      backoff: FiniteDuration
  ): IO[ExitCode] =
    connectOnce.flatMap {
      case SessionOutcome.Quit         => IO.println("Bye!").as(ExitCode.Success)
      case SessionOutcome.Incompatible => IO.pure(ExitCode.Error)
      case SessionOutcome.Lost =>
        if !serviceMode then
          IO.println("Connection lost — restart the client to reconnect.").as(ExitCode.Error)
        else
          IO.println("Connection lost — reconnecting...") *>
            IO.sleep(reconnectInitialBackoff) *>
            reconnectLoop(connectOnce, reconnectInitialBackoff)
      case SessionOutcome.ConnectFailed =>
        if !serviceMode then IO.pure(ExitCode.Error)
        else
          IO.println(s"Reconnecting in ${backoff.toSeconds}s...") *>
            IO.sleep(backoff) *>
            reconnectLoop(connectOnce, (backoff * 2).min(reconnectMaxBackoff))
    }

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
  ): IO[SessionOutcome] = {
    val initialDataIO: IO[String] = for {
      hostname <- getHostname
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
      finalString = List(hostname, s"proto:$protocolVersion", authData)
        .filter(_.nonEmpty)
        .mkString("\n") + "\n\n"
      _ <- logger.debug(s"Prepared initial data to send.")
    } yield finalString

    (
      detectTerminal,
      IO(Option(System.console()).isDefined),
      Mutex[IO],
      Queue.bounded[IO, String](1024),
      Deferred[IO, Either[Throwable, Unit]],
      IO.monotonic.flatMap(t => Ref.of[IO, FiniteDuration](t)),
      IO.realTime.flatMap(t => Ref.of[IO, FiniteDuration](t)),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, String](""),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, Int](0),
      Ref.of[IO, Option[(Int, Int)]](None),
      Ref.of[IO, Option[Voice]](None),
      Ref.of[IO, Option[String]](None),
      Ref.of[IO, Option[String]](None),
      Ref.of[IO, Option[(List[Char], Int)]](None)
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
        input,
        composing,
        blockLines,
        termSize,
        voiceRef,
        hint,
        pendingPaste,
        pasteBuf
      ) = tup
      val ictl = InputCtl(input, hint, pendingPaste, pasteBuf, composing)
      val ui = new Ui(mutex, state, ictl, blockLines, pty, termSize)

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
          myUsername,
          state,
          outgoingQueue,
          ui,
          lastReceived,
          halt,
          incompatible,
          voiceRef
        )
          .onFinalize(logger.debug("Server reader stream finished."))

      val userReader: Stream[IO, Unit] =
        readFromUser(outgoingQueue, halt, state, ui, ictl, voiceRef)
          .onFinalize(logger.debug("User reader stream finished."))

      val pinger: Stream[IO, Unit] =
        Stream
          .awakeEvery[IO](pingInterval)
          .evalMap(_ => outgoingQueue.offer("PING"))

      val typingRefresher: Stream[IO, Unit] =
        Stream.awakeEvery[IO](typingRefreshInterval).evalMap { _ =>
          (composing.get, input.get).flatMapN { (c, inp) =>
            if c && inp.nonEmpty then outgoingQueue.offer("TYPING") else IO.unit
          }
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
          Stream.awakeEvery[IO](1.second).evalMap { _ =>
            detectTerminal.flatMap { latest =>
              termSize.getAndSet(latest).flatMap { previous =>
                if previous == latest then IO.unit
                else
                  (if previous.isEmpty && latest.isDefined then Console[IO].print("\u001b[?2004h")
                   else IO.unit) *> ui.onResize
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
            .guarantee(voiceRef.getAndSet(None).flatMap(_.traverse_(_.teardown)))
            .handleErrorWith { err =>
              connectionLost.set(true) *>
                logger
                  .error(s"\nConnection error: ${Option(err.getMessage).getOrElse(err.toString)}")
            } >>
          (connectionLost.get, incompatible.get).mapN { (lost, incompat) =>
            if incompat then SessionOutcome.Incompatible
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
    line.startsWith("SIGNATURE:") || line.startsWith("/verify ") ||
      line.startsWith("FILEDATA:") || line.startsWith("FILEEND:")

  private def rawMode(pty: Boolean): Resource[IO, Unit] =
    if !pty then Resource.unit[IO]
    else
      Resource
        .make(
          IO.blocking(Seq("sh", "-c", "stty -g < /dev/tty").!!.trim)
            .flatTap(_ =>
              IO.blocking(Seq("sh", "-c", "stty -icanon -echo min 1 time 0 < /dev/tty").!).void
            )
            .flatTap(_ => Console[IO].print("\u001b[?2004h"))
            .handleError(_ => "")
        )(saved =>
          Console[IO].print("\u001b[?2004l").attempt.void *>
            (if saved.isEmpty then IO.unit
             else IO.blocking(Seq("sh", "-c", s"stty $saved < /dev/tty").!).attempt.void)
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
      incompatible: Ref[IO, Boolean],
      voiceRef: Ref[IO, Option[Voice]]
  ): Stream[IO, Nothing] =
    socket.reads
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .evalMap { msg =>
        IO.monotonic.flatMap(lastReceived.set) *> {
          if msg == "PONG" then IO.unit
          else if msg.startsWith("INCOMPATIBLE:") then
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
            val users = msg
              .drop(7)
              .split(",")
              .map(_.trim)
              .filter(_.nonEmpty)
              .filterNot(_.equalsIgnoreCase(myUsername))
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
          else if msg.startsWith("FILEOFFER:") then handleFileOffer(msg, state, ui)
          else if msg.startsWith("FILEACCEPT:") then
            handleFileAccept(msg.drop(11).trim, state, outgoingQueue, ui)
          else if msg.startsWith("FILEREJECT:") then handleFileReject(msg.drop(11).trim, state, ui)
          else if msg.startsWith("FILEDATA:") then handleFileData(msg, state, ui)
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

  private def handleManualChallenge(
      challenge: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String]
  ): IO[Unit] =
    for
      currentState <- state.get
      _ <- currentState.privateKey match
        case Some(privateKey) =>
          allow[Authentication.AuthError] {
            for
              _ <- logger.debug(s"Received authentication challenge, auto-signing...")
              signature <- Authentication.signChallenge(challenge, privateKey)
              _ <- outgoingQueue.offer(s"/verify $signature")
            yield ()
          }.rescue { err =>
            logger.warn(s"Could not sign challenge: ${err.message}")
          }
        case None =>
          logger.error("Cannot sign challenge: SSH private key not loaded")
    yield ()

  private def readFromUser(
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      ictl: InputCtl,
      voiceRef: Ref[IO, Option[Voice]]
  ): Stream[IO, Nothing] =
    if !ui.isTty then
      Stream
        .repeatEval(Console[IO].readLine.map(Option(_)))
        .evalMap {
          case None    => halt.complete(Right(())).void
          case Some(s) => dispatchLine(s.trim, outgoingQueue, halt, state, ui, voiceRef)
        }
        .drain
    else
      tokenize(
        fs2.io
          .stdin[IO](64)
          .through(text.utf8.decode)
          .flatMap(chunk => Stream.emits(chunk.toList))
      )
        .evalMap(tok => handleToken(tok, outgoingQueue, halt, state, ui, ictl, voiceRef))
        .drain

  private def handleToken(
      tok: InputToken,
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      ictl: InputCtl,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    tok match
      case InputToken.PasteStart => ictl.paste.set(Some((Nil, 0)))
      case InputToken.PasteEnd   => finishPaste(outgoingQueue, ui, ictl)
      case InputToken.Ch(c) =>
        ictl.paste.get.flatMap {
          case Some(_) =>
            val ch = if c == '\r' then '\n' else c
            if ch == '\n' || ch == '\t' || ch >= ' ' then
              ictl.paste.update(_.map { (cs, n) =>
                if n < maxPasteChars then (ch :: cs, n + 1) else (cs, n + 1)
              })
            else IO.unit
          case None => handleInputChar(c, outgoingQueue, halt, state, ui, ictl, voiceRef)
        }

  private def finishPaste(
      outgoingQueue: Queue[IO, String],
      ui: Ui,
      ictl: InputCtl
  ): IO[Unit] =
    ictl.paste.getAndSet(None).flatMap {
      case None => IO.unit
      case Some((cs, n)) =>
        val text = cs.reverse.mkString.stripSuffix("\n")
        val lines = text.count(_ == '\n') + 1
        if n > maxPasteChars || lines > maxPasteLines then
          ui.printLine(
            s"Paste dropped: too large (max $maxPasteChars chars / $maxPasteLines lines)."
          )
        else if !text.contains('\n') then
          ictl.text.update(_ + text) *>
            ui.refreshInput *>
            startTyping(outgoingQueue, ictl.composing)
        else
          ictl.pendingPaste.set(Some(text)) *>
            ui.refreshInput *>
            startTyping(outgoingQueue, ictl.composing)
    }

  private def sendPasteBlock(text: String, outgoingQueue: Queue[IO, String]): IO[Unit] =
    val lines = text.split("\n", -1).toList
    val block = s"[paste — ${lines.size} lines]" :: lines.map("│ " + _)
    block.traverse_(l => outgoingQueue.offer(l) *> IO.sleep(pasteLineDelay)).start.void

  private def handleInputChar(
      ch: Char,
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      ictl: InputCtl,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    ch match
      case '\n' | '\r' =>
        (ictl.text.getAndSet(""), ictl.pendingPaste.getAndSet(None), ictl.hint.getAndSet(None))
          .flatMapN { (line, paste, _) =>
            ui.refreshInput *>
              stopTyping(outgoingQueue, ictl.composing) *>
              dispatchLine(line.trim, outgoingQueue, halt, state, ui, voiceRef) *>
              paste.traverse_(sendPasteBlock(_, outgoingQueue))
          }
      case '\t' =>
        completeInput(state, ui, ictl)
      case '\u007f' | '\b' =>
        ictl.hint.set(None) *>
          ictl.pendingPaste.getAndSet(None).flatMap {
            case Some(_) => ui.refreshInput // Backspace discards a pending paste first
            case None =>
              ictl.text
                .updateAndGet { s =>
                  if s.isEmpty then s
                  else s.dropRight(if Character.isLowSurrogate(s.last) then 2 else 1)
                }
                .flatMap { s =>
                  ui.refreshInput *>
                    (if s.isEmpty then stopTyping(outgoingQueue, ictl.composing) else IO.unit)
                }
          }
      case '\u0004' => // Ctrl-D on an empty prompt behaves like /quit
        if serviceMode then ui.printLine(quitHint) else halt.complete(Right(())).void
      case c if c >= ' ' =>
        ictl.hint.set(None) *>
          ictl.text.update(_ + c) *>
          ui.refreshInput *>
          startTyping(outgoingQueue, ictl.composing)
      case _ => IO.unit

  private val clientCommands = List(
    "/acceptfile",
    "/auth",
    "/ban",
    "/help",
    "/kick",
    "/mute",
    "/quit",
    "/rejectfile",
    "/sendfile",
    "/verify",
    "/voice",
    "/voicetest"
  )

  private def splitLastToken(s: String): (String, String) =
    val i = s.lastIndexOf(' ')
    (s.take(i + 1), s.drop(i + 1))

  private def commonPrefix(xs: List[String]): String =
    xs.reduce { (a, b) =>
      a.take(a.zip(b).takeWhile(_ == _).length)
    }

  private def completionFor(inp: String, st: ClientState): IO[List[String]] =
    val (head, token) = splitLastToken(inp)
    if head.isEmpty && token.startsWith("/") then
      IO.pure(clientCommands.filter(_.startsWith(token)))
    else if token.startsWith("@") then
      val p = token.drop(1).toLowerCase
      IO.pure(st.onlineUsers.filter(_.toLowerCase.startsWith(p)).sorted.map("@" + _))
    else if token.startsWith(":") && token.length > 1 then
      IO.pure(
        Emoji.shortcodes.keys.toList.filter(_.startsWith(token.drop(1))).sorted.map(k => s":$k:")
      )
    else if inp.startsWith("/sendfile ") && head.nonEmpty then completePath(token)
    else IO.pure(Nil)

  private def completePath(token: String): IO[List[String]] =
    IO {
      val home = System.getProperty("user.home")
      val expanded =
        if token == "~" then home + "/"
        else if token.startsWith("~/") then home + token.drop(1)
        else token
      val slash = expanded.lastIndexOf('/')
      val (dir, prefix) =
        if slash < 0 then (Paths.get("."), expanded)
        else (Paths.get(expanded.take(slash + 1)), expanded.drop(slash + 1))
      val keep = token.take(token.lastIndexOf('/') + 1)
      (dir, prefix, keep)
    }.flatMap { (dir, prefix, keep) =>
      Fs2Files[IO]
        .list(Fs2Path.fromNioPath(dir))
        .evalMap(p => Fs2Files[IO].isDirectory(p).map(d => (p.fileName.toString, d)))
        .compile
        .toList
        .map(
          _.filter((name, _) => name.startsWith(prefix))
            .sortBy(_._1)
            .map((name, isDir) => keep + name + (if isDir then "/" else ""))
        )
    }.handleError(_ => Nil)

  private def completeInput(state: Ref[IO, ClientState], ui: Ui, ictl: InputCtl): IO[Unit] =
    ictl.pendingPaste.get.flatMap {
      case Some(_) => IO.unit
      case None =>
        (ictl.text.get, state.get).flatMapN { (inp, st) =>
          completionFor(inp, st).flatMap {
            case Nil => ictl.hint.set(None) *> ui.refreshInput
            case single :: Nil =>
              val done = if single.endsWith("/") then single else single + " "
              ictl.text.set(splitLastToken(inp)._1 + done) *>
                ictl.hint.set(None) *>
                ui.refreshInput
            case candidates =>
              val (head, token) = splitLastToken(inp)
              val lcp = commonPrefix(candidates)
              val extended = if lcp.length > token.length then head + lcp else inp
              ictl.text.set(extended) *>
                ictl.hint.set(Some(candidates.mkString("  "))) *>
                ui.refreshInput
          }
        }
    }

  private def dispatchLine(
      line: String,
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    if line.isEmpty then IO.unit
    else if line == "/quit" then
      if serviceMode then ui.printLine(quitHint) else halt.complete(Right(())).void
    else if line.startsWith("/sendfile ") then
      prepareSendFile(line.drop(10), state, outgoingQueue, ui)
    else if line == "/voice" then toggleVoice(state, outgoingQueue, ui, voiceRef)
    else if line == "/voicetest" then toggleVoiceTest(state, outgoingQueue, ui, voiceRef)
    else if line == "/mute" then toggleMute(state, ui, voiceRef)
    else if line.startsWith("/") then outgoingQueue.offer(line)
    else outgoingQueue.offer(Emoji.expand(line))

  private def toggleVoice(
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    voiceRef.get.flatMap {
      case Some(_) => stopVoice(state, outgoingQueue, ui, voiceRef)
      case None    => startVoice(state, outgoingQueue, ui, voiceRef, loopback = false)
    }

  private def toggleVoiceTest(
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    voiceRef.get.flatMap {
      case Some(_) => stopVoice(state, outgoingQueue, ui, voiceRef)
      case None    => startVoice(state, outgoingQueue, ui, voiceRef, loopback = true)
    }

  private def startVoice(
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui,
      voiceRef: Ref[IO, Option[Voice]],
      loopback: Boolean
  ): IO[Unit] =
    allow[Audio.AudioError] {
      Audio.open(state.get.map(_.muted)).allocated.flatMap { case (handle, release) =>
        for
          seq <- Ref.of[IO, Int](0)
          captureFib <- handle.frames
            .evalMap { b64 =>
              if loopback then handle.receive("you (test)", b64)
              else seq.getAndUpdate(_ + 1).flatMap(n => outgoingQueue.offer(s"VOICE:$n:$b64"))
            }
            .compile
            .drain
            .start
          playbackFib <- handle.playback.compile.drain.start
          teardown = playbackFib.cancel *> release *> captureFib.cancel
          _ <- voiceRef.set(Some(Voice(handle, teardown)))
          _ <- state.update(_.copy(inVoice = true, muted = false))
          _ <- if loopback then IO.unit else outgoingQueue.offer("VOICEJOIN")
          _ <- ui.printLine(
            if loopback then
              "Voice self-test on. Speak and you should hear yourself back. " +
                "Use headphones (open speakers will feed back). /mute or /voicetest to stop."
            else
              "Joined voice. Use headphones to avoid echo. /mute toggles your mic, /voice leaves."
          )
        yield ()
      }
    }.rescue { err =>
      ui.printLine(s"Voice unavailable: ${err.message}. Staying in text mode.")
    }

  private def stopVoice(
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    voiceRef.getAndSet(None).flatMap {
      case None => IO.unit
      case Some(voice) =>
        state.update(_.copy(inVoice = false, muted = false, voiceUsers = Nil)) *>
          outgoingQueue.offer("VOICELEAVE") *>
          voice.teardown *>
          ui.setVoiceUsers(Nil) *>
          ui.printLine("Left voice.")
    }

  private def toggleMute(
      state: Ref[IO, ClientState],
      ui: Ui,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    voiceRef.get.flatMap {
      case None => ui.printLine("You're not in voice. Join with /voice first.")
      case Some(_) =>
        state
          .updateAndGet(st => st.copy(muted = !st.muted))
          .flatMap(st => ui.printLine(if st.muted then "Mic muted." else "Mic unmuted."))
    }

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

  private def prepareSendFile(
      rest: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String],
      ui: Ui
  ): IO[Unit] =
    rest.trim.split(" ", 2) match
      case Array(targetToken, rawPath) if targetToken.startsWith("@") =>
        val path = Paths.get(rawPath.trim)
        allow[FileError] {
          for
            checked <- IO.blocking {
              val regular = Files.isRegularFile(path)
              val readable = regular && Files.isReadable(path)
              val size = if regular then Files.size(path) else -1L
              (Files.exists(path), regular, readable, size)
            }.attempt
            size <- checked match
              case Left(err)               => raiseFile(FileError.Unreadable(err.getMessage))
              case Right((false, _, _, _)) => raiseFile(FileError.NotFound(rawPath))
              case Right((_, false, _, _)) => raiseFile(FileError.NotRegular(rawPath))
              case Right((_, _, false, _)) => raiseFile(FileError.NotReadable(rawPath))
              case Right((_, _, _, s)) if s > maxFileSize => raiseFile(FileError.TooLarge(s))
              case Right((_, _, _, s))                    => IO.pure(s)
            id <- UUIDGen[IO].randomUUID.map(_.toString.take(8))
            name = path.getFileName.toString
            _ <- state.update(st =>
              st.copy(outgoingFiles = st.outgoingFiles + (id -> OutgoingFile(path, name, size)))
            )
            _ <- outgoingQueue.offer(s"/sendfile $targetToken $id $size $name")
          yield ()
        }.rescue(e => ui.printLine(fileErrorMessage(e)))
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
    (IO(MessageDigest.getInstance("SHA-256")), Ref.of[IO, Long](0L)).flatMapN { (md, sent) =>
      Fs2Files[IO]
        .readAll(Fs2Path.fromNioPath(out.path))
        .chunkN(fileChunkSize)
        .zipWithIndex
        .evalMap { case (chunk, seq) =>
          val bytes = chunk.toArray
          IO(md.update(bytes)) *>
            outgoingQueue.offer(s"FILEDATA:$id:$seq:${Base64.getEncoder.encodeToString(bytes)}") *>
            sent
              .modify { prev =>
                val now = prev + bytes.length
                (now, progressMilestone(prev, now, out.size))
              }
              .flatMap(_.traverse_(pct => ui.printLine(s"Sending ${out.name}... $pct%")))
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

  private def handleFileData(msg: String, state: Ref[IO, ClientState], ui: Ui): IO[Unit] =
    msg.split(":", 4) match
      case Array(_, id, _, b64) =>
        state.get.map(_.incomingFiles.get(id)).flatMap {
          case None => IO.unit
          case Some(incoming) =>
            val bytes = Base64.getDecoder.decode(b64)
            val write = incoming.temp match
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
            write *>
              state
                .modify { st =>
                  st.incomingFiles.get(id) match
                    case None => (st, None)
                    case Some(inc) =>
                      val now = inc.received + bytes.length
                      (
                        st.copy(incomingFiles =
                          st.incomingFiles.updated(id, inc.copy(received = now))
                        ),
                        progressMilestone(inc.received, now, inc.size).map((inc, _))
                      )
                }
                .flatMap(_.traverse_ { (inc, pct) =>
                  ui.printLine(s"Receiving ${inc.name} from ${inc.from}... $pct%")
                })
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
    allow[FileError] {
      streamSha256(tmp).flatMap { actualSha =>
        if !actualSha.equalsIgnoreCase(expectedSha) then
          IO.blocking(Files.deleteIfExists(tmp)).attempt.void *>
            raiseFile(FileError.ChecksumMismatch(incoming.name))
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
    }.rescue(e => ui.printLine(fileErrorMessage(e)))

  private def toHex(bytes: Array[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString

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
      LazyList
        .from(1)
        .map(i => dir.resolve(s"$base ($i)$ext"))
        .find(p => !Files.exists(p))
        .getOrElse(initial)
