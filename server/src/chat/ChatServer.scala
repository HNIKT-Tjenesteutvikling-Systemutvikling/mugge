package chat

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.*
import fs2.io.net.*
import com.comcast.ip4s.*
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class Client(
    id: String,
    queue: Queue[IO, Message],
    socket: Socket[IO]
)

object ChatServer extends IOApp:

  private val port = port"5555"
  private val host = host"0.0.0.0"

  def run(args: List[String]): IO[ExitCode] =
    for
      _ <- IO.println(s"Starting chat server on $host:$port")
      clients <- Ref.of[IO, Map[String, Client]](Map.empty)
      _ <- Network[IO]
        .server(Some(host), Some(port))
        .map { clientSocket =>
          handleClient(clientSocket, clients)
            .handleErrorWith(err => IO.println(s"Client error: ${err.getMessage}"))
        }
        .parJoinUnbounded
        .compile
        .drain
    yield ExitCode.Success

  private def handleClient(socket: Socket[IO], clients: Ref[IO, Map[String, Client]]): IO[Unit] =
    for
      clientId <- IO(java.util.UUID.randomUUID().toString.take(8))
      queue <- Queue.unbounded[IO, Message]
      client = Client(clientId, queue, socket)
      _ <- clients.update(_ + (clientId -> client))
      _ <- IO.println(s"Client $clientId connected")
      _ <- socket.write(Chunk.from(s"Welcome! Your ID is: $clientId\n".getBytes))
      _ <- (
        readFromClient(client, clients)
          .concurrently(
            writeToClient(client)
          )
        )
        .guarantee(
          removeClient(clientId, clients)
        )
    yield ()

  private def readFromClient(client: Client, clients: Ref[IO, Map[String, Client]]): IO[Unit] =
    client.socket.reads
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .evalMap { line =>
        val message = Message(
          timestamp = LocalDateTime.now(),
          clientId = client.id,
          content = line
        )
        broadcastMessage(message, clients)
      }
      .compile
      .drain

  private def writeToClient(client: Client): IO[Unit] =
    Stream
      .fromQueueUnterminated(client.queue)
      .map(formatMessage)
      .through(text.utf8.encode)
      .through(client.socket.writes)
      .compile
      .drain

  private def formatMessage(message: Message): String =
    val timeStr = message.timestamp.format(DateTimeFormatter.ofPattern("HH:mm:ss"))
    s"[$timeStr] ${message.clientId}: ${message.content}\n"

  private def broadcastMessage(message: Message, clients: Ref[IO, Map[String, Client]]): IO[Unit] =
    for
      currentClients <- clients.get
      _ <- currentClients.values.toList.traverse_ { client =>
        client.queue.offer(message).handleErrorWith(_ => IO.unit)
      }
      _ <- IO.println(formatMessage(message).trim)
    yield ()

  private def removeClient(clientId: String, clients: Ref[IO, Map[String, Client]]): IO[Unit] =
    for
      _ <- clients.update(_ - clientId)
      _ <- IO.println(s"Client $clientId disconnected")
      remainingClients <- clients.get
      disconnectMessage = Message(
        timestamp = LocalDateTime.now(),
        clientId = "SERVER",
        content = s"Client $clientId has left the chat"
      )
      _ <-
        if remainingClients.nonEmpty then broadcastMessage(disconnectMessage, clients)
        else IO.unit
    yield ()
