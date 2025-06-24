package chat

import cats.effect.*
import cats.effect.std.{Console, Queue}
import cats.syntax.all.*
import fs2.*
import fs2.io.net.*
import com.comcast.ip4s.*
import java.io.IOException

object ChatClient extends IOApp:

  private val serverPort = port"5555"
  private val serverHost = host"localhost"

  def run(args: List[String]): IO[ExitCode] =
    val host = args.headOption.map(Host.fromString).flatten.getOrElse(serverHost)
    val port = args.lift(1).flatMap(Port.fromString).getOrElse(serverPort)

    Network[IO]
      .client(SocketAddress(host, port))
      .use { socket =>
        for
          _ <- IO.println(s"Connected to chat server at $host:$port")
          _ <- IO.println("Type your messages and press Enter to send. Type '/quit' to exit.")
          _ <- handleConnection(socket)
        yield ExitCode.Success
      }
      .handleErrorWith {
        case _: IOException =>
          IO.println(s"Failed to connect to server at $host:$port") *> IO(ExitCode.Error)
        case err =>
          IO.println(s"Error: ${err.getMessage}") *> IO(ExitCode.Error)
      }

  private def handleConnection(socket: Socket[IO]): IO[Unit] =
    Queue.unbounded[IO, String].flatMap { outgoingQueue =>
      val readFromServer = socket.reads
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(_.nonEmpty)
        .evalMap(msg => IO.println(s"\r$msg"))
        .compile
        .drain

      val readFromUser = Stream
        .repeatEval(Console[IO].readLine)
        .takeWhile(_ != "/quit")
        .evalMap(line => outgoingQueue.offer(line))
        .compile
        .drain

      val writeToServer = Stream
        .fromQueueUnterminated(outgoingQueue)
        .map(_ + "\n")
        .through(text.utf8.encode)
        .through(socket.writes)
        .compile
        .drain

      readFromServer
        .both(readFromUser)
        .both(writeToServer)
        .void
        .handleErrorWith { err =>
          IO.println(s"\nConnection error: ${err.getMessage}")
        }
    }
