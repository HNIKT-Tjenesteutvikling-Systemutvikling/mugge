package chat

import cats.effect.*
import cats.effect.std.Console
import cats.mtl.Handle.allow
import com.comcast.ip4s.*
import fs2.io.net.*
import org.typelevel.log4cats.Logger as TLogger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.io.IOException
import scala.concurrent.duration.*

object ChatClient extends IOApp:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  private val reconnectInitialBackoff = 2.seconds
  private val reconnectMaxBackoff = 30.seconds

  def run(args: List[String]): IO[ExitCode] =
    val insecureTls =
      args.contains("--insecure-tls") || sys.env.get("MUGGE_INSECURE_TLS").contains("1")
    args.indexOf("--assist") match
      case idx if idx >= 0 =>
        args.lift(idx + 1) match
          case Some(target) => AssistBridge.run(target, insecureTls)
          case None =>
            Console[IO].errorln("Usage: mugge-client --assist <user>").as(ExitCode.Error)
      case _ => runInteractive(args, insecureTls)

  private def runInteractive(args: List[String], insecureTls: Boolean): IO[ExitCode] =
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
      myUsername <- Config.username
      githubUsername <- allow[Authentication.AuthError] {
        Authentication.detectGithubUsername()
      }.rescue { err =>
        logger.warn(s"Could not detect GitHub username: ${err.message}").as(None)
      }
      _ <- logger.info(s"The github username is: ${githubUsername}")
      _ <- githubUsername match
        case Some(ghu) => logger.debug(s"Detected GitHub username: $ghu")
        case None      => logger.error("Could not detect GitHub username from git config")

      _ <- IO.whenA(insecureTls)(IO.println(Config.insecureTlsNotice))
      tlsContext <- if insecureTls then Network[IO].tlsContext.insecure else Tls.pinnedContext
      // Outlives a session so input history survives the reconnect loop.
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
                state <- Ref.of[IO, ClientState](
                  ClientState(
                    username = myUsername,
                    githubUsername = githubUsername
                  )
                )
                outcome <- Session.run(socket, state, inputHistory)
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
      exitCode <- reconnectLoop(connectOnce, reconnectInitialBackoff)
    yield exitCode

  private def reconnectLoop(
      connectOnce: IO[SessionOutcome],
      backoff: FiniteDuration
  ): IO[ExitCode] =
    connectOnce.flatMap {
      case SessionOutcome.Quit         => IO.println("Bye!").as(ExitCode.Success)
      case SessionOutcome.Incompatible => IO.pure(ExitCode.Error)
      // Refusal is terminal: exit clean so service mode does not respawn-loop.
      case SessionOutcome.Denied => IO.pure(ExitCode.Success)
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
