package chat

import cats.effect.*
import cats.effect.std.{Console, Queue}
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
      privateKey: Option[PrivateKey] = None
  )

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
            _ <- logger.info(s"Connected to chat server at $host:$port!")
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

  private def getUsername: IO[String] =
    IO.blocking(java.net.InetAddress.getLocalHost.getHostName)
      .map(UserMapping.mapHostname)

  private def handleConnection(
      socket: Socket[IO],
      myUsername: String,
      state: Ref[IO, ClientState]
  ): IO[Unit] = {
    val initialDataIO: IO[String] = for {
      hostname <- IO
        .blocking(java.net.InetAddress.getLocalHost.getHostName)
        .handleError(_ => "unknown-client")
      currentState <- state.get
      privateKey <- currentState.githubUsername.flatTraverse { _ =>
        Authentication.loadPrivateKey().map(_.toOption).handleErrorWith { err =>
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

    Queue.unbounded[IO, String].flatMap { outgoingQueue =>
      val serverWriter: Stream[IO, Nothing] =
        (Stream.eval(initialDataIO) ++ Stream.fromQueueUnterminated(outgoingQueue))
          .map(_ + "\n")
          .evalTap(data => logger.info(s"Writing to server: $data"))
          .through(text.utf8.encode)
          .through(socket.writes)
          .onFinalize(logger.info("Server writer stream finished."))

      val serverReader: Stream[IO, Unit] =
        readFromServer(socket, myUsername, state, outgoingQueue)
          .onFinalize(logger.info("Server reader stream finished."))

      val userReader: Stream[IO, Unit] =
        readFromUser(outgoingQueue)
          .onFinalize(logger.info("User reader stream finished."))

      logger.info("Starting chat streams...") >>
        Stream(serverReader, serverWriter, userReader).parJoinUnbounded.compile.drain
          .handleErrorWith { err =>
            logger.error(s"\nConnection error: ${Option(err.getMessage).getOrElse(err.toString)}")
          }
    }
  }

  private def readFromServer(
      socket: Socket[IO],
      myUsername: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String]
  ): Stream[IO, Nothing] =
    socket.reads
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .evalMap { msg =>
        if msg.startsWith("CHALLENGE:") then handleAutoChallenge(msg.drop(10), state, outgoingQueue)
        else if msg.startsWith("Challenge: ") then
          handleManualChallenge(msg.drop(11), state, outgoingQueue)
        else if msg.contains("Authentication successful!") then
          state.update(_.copy(authenticated = true)) >>
            IO.println(s"\r$msg") >>
            logger.info("You can now start chatting!")
        else
          IO.println(s"\r$msg") >>
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

  private def readFromUser(outgoingQueue: Queue[IO, String]): Stream[IO, Nothing] =
    Stream
      .repeatEval(Console[IO].readLine)
      .takeWhile(s => s != null && s != "/quit")
      .foreach(outgoingQueue.offer)
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
