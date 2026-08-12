package chat

import cats.effect.*
import cats.effect.std.Console
import cats.mtl.Handle.allow
import com.comcast.ip4s.*
import fs2.io.net.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.io.IOException
import scala.concurrent.duration.*

object ChatClient extends IOApp:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger = LoggerFactory[IO].getLogger

  private val reconnectInitialBackoff = 2.seconds
  private val reconnectMaxBackoff = 30.seconds

  def run(args: List[String]): IO[ExitCode] =
    val insecureTls =
      args.contains("--insecure-tls") || sys.env.get("MUGGE_INSECURE_TLS").contains("1")

    val ansi = LiveAnsi()
    val emoji = LiveEmoji()
    val markup = LiveMarkup()
    val highlighter = LiveHighlighter()
    val userMapping = LiveUserMapping()
    val tokenizer = LiveTokenizer[IO]()
    val config = LiveConfig[IO](userMapping)
    val terminal = LiveTerminal[IO]()
    val tls = LiveTls[IO]()
    val authentication = LiveAuthentication[IO]()
    val audio = LiveAudio[IO]()
    val idle = LiveIdle[IO]()
    val notifications = LiveNotifications[IO](audio)
    val whisper = LiveWhisper[IO](ansi, notifications)
    val completion = LiveCompletion[IO]()
    val fileTransfer = LiveFileTransfer[IO](notifications)
    val voice = LiveVoice[IO](audio)
    val assist = LiveAssist[IO](notifications, emoji)
    val userInput =
      LiveUserInput[IO](completion, fileTransfer, voice, assist, markup, emoji, tokenizer)
    val session = LiveSession[IO](
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
    val assistBridge = LiveAssistBridge[IO](config, authentication, tls)

    for
      ipc <- LiveIpc.create[IO]
      exitCode <- args.indexOf("--assist") match
        case idx if idx >= 0 =>
          args.lift(idx + 1) match
            case Some(target) => assistBridge.run(target, insecureTls)
            case None =>
              Console[IO].errorln("Usage: mugge-client --assist <user>").as(ExitCode.Error)
        case _ => runInteractive(args, insecureTls, config, authentication, tls, session, ipc)
    yield exitCode

  private def runInteractive(
      args: List[String],
      insecureTls: Boolean,
      config: Config[IO],
      authentication: Authentication[IO],
      tls: Tls[IO],
      session: Session[IO],
      ipc: Ipc[IO]
  ): IO[ExitCode] =
    val positional = args.filterNot(_.startsWith("--"))
    val host = positional.headOption
      .flatMap(Host.fromString)
      .orElse(sys.env.get("CHAT_SERVER_HOST").flatMap(Host.fromString))
      .getOrElse(Config.defaultHost)

    val port = positional
      .lift(1)
      .flatMap(Port.fromString)
      .orElse(sys.env.get("CHAT_SERVER_PORT").flatMap(Port.fromString))
      .getOrElse(Config.defaultPort)

    for
      _ <- logger.info(s"Mugge Chat Client starting...")
      _ <- logger.info(s"Server: $host:$port")
      myUsername <- config.username
      githubUsername <- allow[Authentication.AuthError] {
        authentication.detectGithubUsername()
      }.rescue { err =>
        logger.warn(s"Could not detect GitHub username: ${err.message}").as(None)
      }
      _ <- logger.info(s"The github username is: ${githubUsername}")
      _ <- githubUsername match
        case Some(ghu) => logger.debug(s"Detected GitHub username: $ghu")
        case None =>
          logger.error(
            s"Could not detect GitHub username. ${Authentication.githubUserHint}"
          ) *> IO.println(
            s"Could not detect your GitHub username. ${Authentication.githubUserHint}"
          )

      _ <- IO.whenA(insecureTls)(IO.println(Config.insecureTlsNotice))
      tlsContext <- if insecureTls then Network[IO].tlsContext.insecure else tls.pinnedContext
      inputHistory <- Ref.of[IO, InputHistory](InputHistory.empty)
      connectOnce = Network[IO]
        .connect(SocketAddress(host, port))
        .use { rawSocket =>
          tlsContext
            .clientBuilder(rawSocket)
            .withParameters(Tls.parameters)
            .build
            .use { socket =>
              for
                _ <- IO.println(s"Connected to chat server at $host:$port")
                state <- Ref.of[IO, ClientState[IO]](
                  ClientState[IO](
                    username = myUsername,
                    githubUsername = githubUsername
                  )
                )
                outcome <- session.run(socket, state, inputHistory, ipc)
              yield outcome
            }
        }
        .handleErrorWith { err =>
          if Tls.isPinMismatch(err) then
            IO.println(Config.pinMismatchNotice).as(SessionOutcome.Denied)
          else
            err match
              case _: IOException =>
                logger
                  .error(s"Failed to connect to server at $host:$port")
                  .as(SessionOutcome.ConnectFailed)
              case _ =>
                logger.error(s"Error: ${err.getMessage}").as(SessionOutcome.ConnectFailed)
        }
      exitCode <- ipc.serve.use(_ => reconnectLoop(connectOnce, reconnectInitialBackoff))
    yield exitCode

  private def reconnectLoop(
      connectOnce: IO[SessionOutcome],
      backoff: FiniteDuration
  ): IO[ExitCode] =
    connectOnce.flatMap {
      case SessionOutcome.Quit         => IO.println("Bye!").as(ExitCode.Success)
      case SessionOutcome.Incompatible => IO.pure(ExitCode.Error)
      case SessionOutcome.Denied       => IO.pure(ExitCode.Success)
      case SessionOutcome.Lost =>
        if !Config.serviceMode then
          IO.println("Connection lost — restart the client to reconnect.").as(ExitCode.Error)
        else
          IO.println("Connection lost — reconnecting...") *>
            IO.sleep(reconnectInitialBackoff) *>
            reconnectLoop(connectOnce, reconnectInitialBackoff)
      case SessionOutcome.ConnectFailed =>
        if !Config.serviceMode then IO.pure(ExitCode.Error)
        else
          IO.println(s"Reconnecting in ${backoff.toSeconds}s...") *>
            IO.sleep(backoff) *>
            reconnectLoop(connectOnce, (backoff * 2).min(reconnectMaxBackoff))
    }
