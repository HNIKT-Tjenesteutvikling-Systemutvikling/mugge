package chat

import cats.effect.Sync
import cats.syntax.all.*
import com.comcast.ip4s.*

import scala.concurrent.duration.*

trait Config[F[_]]:
  def hostname: F[String]
  def username: F[String]

object Config:
  val defaultPort = port"20222"
  val defaultHost = host"localhost"

  val pinMismatchNotice =
    "Server key does not match the pinned key — possible MITM or key " +
      "rotation; update the client."

  val insecureTlsNotice =
    "⚠ --insecure-tls: the server certificate is NOT verified. Local dev only."

  val protocolVersion = 9

  val serviceMode: Boolean = sys.env.get("MUGGE_SERVICE").contains("1")

  val noAssist: Boolean = sys.env.get("MUGGE_NO_ASSIST").contains("1")

  val ipcEnabled: Boolean = !sys.env.get("MUGGE_NO_IPC").contains("1")

  val ipcSocketPath: String =
    sys.env
      .get("MUGGE_IPC_SOCKET")
      .orElse(sys.env.get("XDG_RUNTIME_DIR").map(dir => s"$dir/mugge-ipc.sock"))
      .getOrElse(s"/tmp/mugge-ipc-${sys.props.getOrElse("user.name", "user")}.sock")

  val quitHint =
    "/quit is disabled here — this chat runs in the background. Press Ctrl-\\ " +
      "(or just close the terminal) to leave without disconnecting. To stop it " +
      "entirely: systemctl --user stop mugge-chat (Guix: herd stop mugge-chat)"

  val pingInterval = 60.seconds

final class LiveConfig[F[_]: Sync] private (userMapping: UserMapping) extends Config[F]:
  override def hostname: F[String] =
    sys.env.get("MUGGE_HOSTNAME") match
      case Some(name) => name.pure[F]
      case None =>
        Sync[F]
          .blocking(java.net.InetAddress.getLocalHost.getHostName)
          .handleError(_ => "unknown-client")

  override def username: F[String] =
    hostname.map(userMapping.mapHostname)

object LiveConfig:
  def apply[F[_]: Sync](userMapping: UserMapping): Config[F] = new LiveConfig[F](userMapping)
