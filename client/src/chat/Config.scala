package chat

import cats.effect.IO
import com.comcast.ip4s.*

import scala.concurrent.duration.*

object Config:
  val defaultPort = port"20222"
  val defaultHost = host"localhost"

  val pinMismatchNotice =
    "Server key does not match the pinned key — possible MITM or key " +
      "rotation; update the client."

  val insecureTlsNotice =
    "⚠ --insecure-tls: the server certificate is NOT verified. Local dev only."

  // Wire-protocol version sent to the server on connect. Bump in BOTH repos
  // when a change makes older clients incompatible; the server refuses any
  // client below its required minimum with an update-and-rebuild message.
  val protocolVersion = 8

  val serviceMode: Boolean = sys.env.get("MUGGE_SERVICE").contains("1")

  val noAssist: Boolean = sys.env.get("MUGGE_NO_ASSIST").contains("1")

  val quitHint =
    "/quit is disabled here — this chat runs in the background. Press Ctrl-\\ " +
      "(or just close the terminal) to leave without disconnecting. To stop it " +
      "entirely: systemctl --user stop mugge-chat"

  val pingInterval = 60.seconds

  def hostname: IO[String] =
    sys.env.get("MUGGE_HOSTNAME") match
      case Some(name) => IO.pure(name)
      case None =>
        IO.blocking(java.net.InetAddress.getLocalHost.getHostName)
          .handleError(_ => "unknown-client")

  def username: IO[String] =
    hostname.map(UserMapping.mapHostname)
