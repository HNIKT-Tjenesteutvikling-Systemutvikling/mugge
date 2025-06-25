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

object ChatClient extends IOApp:

  private val serverPort = port"5555"
  private val serverHost = host"localhost"

  def run(args: List[String]): IO[ExitCode] =
    val host = args.headOption.map(Host.fromString).flatten.getOrElse(serverHost)
    val port = args.lift(1).flatMap(Port.fromString).getOrElse(serverPort)

    for
      myUsername <- getUsername
      exitCode <- Network[IO]
        .client(SocketAddress(host, port))
        .use { socket =>
          for
            _ <- IO.println(s"Connected to chat server at $host:$port")
            _ <- IO.println(s"You are logged in as: $myUsername")
            _ <- IO.println("Type your messages and press Enter to send. Type '/quit' to exit.")
            _ <- IO.println(
              "Use !ping @username [count] to send critical notifications (default: 1, max: 10)."
            )
            _ <- handleConnection(socket, myUsername)
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

  private def handleConnection(socket: Socket[IO], myUsername: String): IO[Unit] =
    for
      _ <- sendHostname(socket)
      _ <- handleChat(socket, myUsername)
    yield ()

  private def sendHostname(socket: Socket[IO]): IO[Unit] =
    for
      hostname <- IO
        .blocking(java.net.InetAddress.getLocalHost.getHostName)
        .handleError(_ => "unknown-client")
      _ <- Stream
        .emit(hostname + "\n")
        .through(text.utf8.encode)
        .through(socket.writes)
        .compile
        .drain
    yield ()

  private def handleChat(socket: Socket[IO], myUsername: String): IO[Unit] =
    Queue.unbounded[IO, String].flatMap { outgoingQueue =>
      val serverReader = readFromServer(socket, myUsername)
      val userReader = readFromUser(outgoingQueue)
      val serverWriter = writeToServer(socket, outgoingQueue)

      serverReader.both(userReader).both(serverWriter).void.handleErrorWith { err =>
        IO.println(s"\nConnection error: ${err.getMessage}")
      }
    }

  private def readFromServer(socket: Socket[IO], myUsername: String): IO[Unit] =
    socket.reads
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .evalMap { msg =>
        for
          _ <- IO.println(s"\r$msg")
          _ <- checkForMentions(msg, myUsername)
          _ <- checkForPings(msg, myUsername)
        yield ()
      }
      .compile
      .drain

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
    val messagePattern = """^\[(\d{2}:\d{2}:\d{2})\] ([^:]+): (.+)$""".r
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
    val messagePattern = """^\[(\d{2}:\d{2}:\d{2})\] ([^:]+): (.+)$""".r
    val pingPattern = """^!ping\s+@(\w+)(?:\s+(\d+))?""".r

    line match
      case messagePattern(time, sender, content) =>
        content match
          case pingPattern(targetUser, countStr) =>
            if targetUser.equalsIgnoreCase(myUsername) then
              val count = Option(countStr).flatMap(_.toIntOption).getOrElse(1)
              val limitedCount = count.min(10).max(1) // Limit between 1 and 10

              sendMultiplePings(sender, time, limitedCount)
            else IO.unit
          case _ => IO.unit
      case _ =>
        IO.unit

  private def sendMultiplePings(sender: String, time: String, count: Int): IO[Unit] =
    val delay = 500.milliseconds

    (1 to count).toList.traverse_ { i =>
      sendNotification(
        title = s"🔔 PING from $sender (${i}/$count)",
        body = s"$sender pinged you at $time",
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
