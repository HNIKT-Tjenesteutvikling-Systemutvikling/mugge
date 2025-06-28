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

object ChatClient extends IOApp:

  private val serverPort = port"5555"
  private val serverHost = host"localhost"

  case class ClientState(
      authenticated: Boolean = false,
      githubUsername: Option[String] = None,
      privateKey: Option[PrivateKey] = None
  )

  def run(args: List[String]): IO[ExitCode] =
    val host = args.headOption.map(Host.fromString).flatten.getOrElse(serverHost)
    val port = args.lift(1).flatMap(Port.fromString).getOrElse(serverPort)

    for
      myUsername <- getUsername
      githubUsername <- Authentication.detectGithubUsername().flatMap(IO.fromEither)
      _ <- githubUsername match
        case Some(ghu) => IO.println(s"Detected GitHub username: $ghu")
        case None      => IO.println("Could not detect GitHub username from git config")

      exitCode <- Network[IO]
        .client(SocketAddress(host, port))
        .use { socket =>
          for
            _ <- IO.println(s"Connecting to chat server at $host:$port...")
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
            IO.println(s"Failed to connect to server at $host:$port") *> IO(ExitCode.Error)
          case err =>
            IO.println(s"Error: ${err.getMessage}") *> IO(ExitCode.Error)
        }
    yield exitCode

  private def getUsername: IO[String] =
    IO.blocking(java.net.InetAddress.getLocalHost.getHostName)
      .map(UserMapping.mapHostname)

  private def handleConnection(
      socket: Socket[IO],
      myUsername: String,
      state: Ref[IO, ClientState]
  ): IO[Unit] =
    for
      _ <- sendInitialData(socket, state)
      _ <- handleChat(socket, myUsername, state)
    yield ()

  private def sendInitialData(socket: Socket[IO], state: Ref[IO, ClientState]): IO[Unit] =
    for
      hostname <- IO
        .blocking(java.net.InetAddress.getLocalHost.getHostName)
        .handleError(_ => "unknown-client")
      currentState <- state.get

      privateKey <- currentState.githubUsername match
        case Some(ghu) =>
          Authentication
            .loadPrivateKey()
            .map(_.toOption)
            .handleError { err =>
              println(s"Could not load SSH private key: ${err.getMessage}")
              None
            }
        case None => IO.pure(None)

      _ <- privateKey match
        case Some(key) => state.update(_.copy(privateKey = Some(key)))
        case None      => IO.unit

      authData = currentState.githubUsername match
        case Some(ghu) if privateKey.isDefined => s"\nauto-auth:$ghu"
        case _                                 => ""

      _ <- Stream
        .emit(s"$hostname$authData\n")
        .through(text.utf8.encode)
        .through(socket.writes)
        .compile
        .drain
    yield ()

  private def handleChat(
      socket: Socket[IO],
      myUsername: String,
      state: Ref[IO, ClientState]
  ): IO[Unit] =
    Queue.unbounded[IO, String].flatMap { outgoingQueue =>
      val serverReader = readFromServer(socket, myUsername, state, outgoingQueue)
      val userReader = readFromUser(outgoingQueue)
      val serverWriter = writeToServer(socket, outgoingQueue)

      serverReader.both(userReader).both(serverWriter).void.handleErrorWith { err =>
        IO.println(s"\nConnection error: ${err.getMessage}")
      }
    }

  private def readFromServer(
      socket: Socket[IO],
      myUsername: String,
      state: Ref[IO, ClientState],
      outgoingQueue: Queue[IO, String]
  ): IO[Unit] =
    socket.reads
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .evalMap { msg =>
        if msg.startsWith("CHALLENGE:") then handleAutoChallenge(msg.drop(10), state, socket)
        else if msg.startsWith("Challenge: ") then
          handleManualChallenge(msg.drop(11), state, outgoingQueue)
        else if msg.contains("Authentication successful!") then
          state.update(_.copy(authenticated = true)) >>
            IO.println(s"\r$msg") >>
            IO.println("You can now start chatting!")
        else
          for
            _ <- IO.println(s"\r$msg")
            _ <- checkForMentions(msg, myUsername)
            _ <- checkForPings(msg, myUsername)
          yield ()
      }
      .compile
      .drain

  private def handleAutoChallenge(
      challenge: String,
      state: Ref[IO, ClientState],
      socket: Socket[IO]
  ): IO[Unit] =
    for
      currentState <- state.get
      _ <- (currentState.privateKey, currentState.githubUsername) match
        case (Some(privateKey), Some(githubUsername)) =>
          for
            _ <- IO.println(s"Received auto-auth challenge, signing...")
            signature <- Authentication.signChallenge(challenge, privateKey).flatMap(IO.fromEither)
            _ <- Stream
              .emit(s"SIGNATURE:$signature\n")
              .through(text.utf8.encode)
              .through(socket.writes)
              .compile
              .drain
            _ <- IO.println(s"Auto-authentication response sent for $githubUsername")
          yield ()
        case _ =>
          IO.println("Cannot auto-authenticate: missing private key or GitHub username")
    yield ()

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
            _ <- IO.println(s"Received authentication challenge, auto-signing...")
            signature <- Authentication.signChallenge(challenge, privateKey).flatMap(IO.fromEither)
            _ <- outgoingQueue.offer(s"/verify $signature")
          yield ()
        case None =>
          IO.println("Cannot sign challenge: SSH private key not loaded")
    yield ()

  private def readFromUser(outgoingQueue: Queue[IO, String]): IO[Unit] =
    Stream
      .repeatEval(Console[IO].readLine)
      .takeWhile(_ != "/quit")
      .evalMap(line => outgoingQueue.offer(line))
      .compile
      .drain

  private def writeToServer(socket: Socket[IO], outgoingQueue: Queue[IO, String]): IO[Unit] =
    Stream
      .fromQueueUnterminated(outgoingQueue)
      .map(_ + "\n")
      .through(text.utf8.encode)
      .through(socket.writes)
      .compile
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
  ): IO[Unit] =
    IO.blocking {
      try
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
        val _ = command.!
      catch
        case e: Exception =>
          println(s"[Notification Error] ${e.getMessage}")
          println(s"[Notification] $title: $body")
    }.void
