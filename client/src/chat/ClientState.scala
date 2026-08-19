package chat

import java.security.PrivateKey
import scala.concurrent.duration.FiniteDuration

final case class ClientState[F[_]](
    username: String = "",
    githubUsername: Option[String] = None,
    privateKey: Option[PrivateKey] = None,
    colors: Map[String, Int] = Map.empty,
    serverColors: Map[String, ColorSpec] = Map.empty,
    onlineUsers: List[String] = Nil,
    typingUsers: List[String] = Nil,
    statuses: Map[String, String] = Map.empty,
    outgoingFiles: Map[String, OutgoingFile] = Map.empty,
    incomingFiles: Map[String, IncomingFile] = Map.empty,
    pingHistory: Map[String, List[FiniteDuration]] = Map.empty,
    inVoice: Boolean = false,
    muted: Boolean = false,
    voiceUsers: List[String] = Nil,
    assistSessions: Map[String, AssistSession[F]] = Map.empty[String, AssistSession[F]],
    pendingAssist: List[(String, String)] = Nil,
    isAdmin: Boolean = false,
    adminMuted: Boolean = false
)
