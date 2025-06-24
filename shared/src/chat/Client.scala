package chat

import cats.effect.*
import cats.effect.std.Queue
import fs2.io.net.*

case class Client(
    id: String,
    queue: Queue[IO, Message],
    socket: Socket[IO]
)
