package chat

import java.time.LocalDateTime

case class Message(
    timestamp: LocalDateTime,
    clientId: String,
    content: String
)
