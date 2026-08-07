package chat

import cats.Applicative
import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.UnixSocketAddress
import fs2.*
import fs2.concurrent.Topic
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.io.net.Network
import fs2.io.net.Socket
import org.typelevel.log4cats.LoggerFactory

/** Local companion socket: JSON-line events out, plain input lines in.
  *
  * External frontends (the VS Code sidebar, editors) attach here rather than opening their own
  * connection, because the server displaces an older session when the same user reconnects — a
  * second client would fight the always-on one.
  */
trait Ipc[F[_]]:
  def message(
      time: String,
      indicator: String,
      sender: String,
      text: String,
      history: Boolean
  ): F[Unit]

  def codeMessage(
      time: String,
      indicator: String,
      sender: String,
      lang: String,
      lines: List[String],
      history: Boolean
  ): F[Unit]

  def whisper(time: String, incoming: Boolean, peer: String, text: String): F[Unit]
  def notice(text: String): F[Unit]
  def users(list: List[String]): F[Unit]
  def typing(list: List[String]): F[Unit]
  def me(name: String): F[Unit]

  /** Holds the input sink of the live session; frontends can only send while it is bound. */
  def bind(send: String => F[Unit]): Resource[F, Unit]

  def serve: Resource[F, Unit]

object Ipc:
  /** Bumped when the event shape changes incompatibly. */
  val ipcProtocol = 1

  final case class Snapshot(
      me: String,
      users: List[String],
      typing: List[String],
      connected: Boolean
  )

  /** Just enough JSON to render flat event objects; no parser is needed on this side. */
  object Json:
    def str(s: String): String =
      val sb = new StringBuilder("\"")
      s.foreach {
        case '"'          => sb ++= "\\\""
        case '\\'         => sb ++= "\\\\"
        case '\n'         => sb ++= "\\n"
        case '\r'         => sb ++= "\\r"
        case '\t'         => sb ++= "\\t"
        case c if c < ' ' => sb ++= f"\\u${c.toInt}%04x"
        case c            => sb += c
      }
      sb += '"'
      sb.result()

    def arr(xs: List[String]): String = xs.map(str).mkString("[", ",", "]")

    def bool(b: Boolean): String = if b then "true" else "false"

    def num(n: Int): String = n.toString

    def obj(fields: (String, String)*): String =
      fields.map((k, v) => s"${str(k)}:$v").mkString("{", ",", "}")

final class LiveIpc[F[_]: Async: Network: Files: LoggerFactory] private (
    enabled: Boolean,
    topic: Topic[F, String],
    recent: Ref[F, Vector[String]],
    snapshot: Ref[F, Ipc.Snapshot],
    sink: Ref[F, Option[String => F[Unit]]]
) extends Ipc[F]:
  import Ipc.*
  import Ipc.Json.*

  private val logger = LoggerFactory[F].getLogger

  private val recentSize = 300
  private val subscriberQueue = 1024
  private val maxClients = 8

  private val ansiPattern = "\u001b\\[[0-9;?]*[ -/]*[@-~]".r

  override def message(
      time: String,
      indicator: String,
      sender: String,
      text: String,
      history: Boolean
  ): F[Unit] =
    emit(
      obj(
        "type" -> str("message"),
        "time" -> str(time),
        "sender" -> str(sender),
        "verified" -> bool(indicator == "✓"),
        "text" -> str(stripAnsi(text)),
        "history" -> bool(history)
      )
    )

  override def codeMessage(
      time: String,
      indicator: String,
      sender: String,
      lang: String,
      lines: List[String],
      history: Boolean
  ): F[Unit] =
    emit(
      obj(
        "type" -> str("message"),
        "time" -> str(time),
        "sender" -> str(sender),
        "verified" -> bool(indicator == "✓"),
        "text" -> str(lines.map(stripAnsi).mkString("\n")),
        "lang" -> str(lang),
        "code" -> bool(true),
        "history" -> bool(history)
      )
    )

  override def whisper(time: String, incoming: Boolean, peer: String, text: String): F[Unit] =
    emit(
      obj(
        "type" -> str("whisper"),
        "time" -> str(time),
        "incoming" -> bool(incoming),
        "peer" -> str(peer),
        "text" -> str(stripAnsi(text))
      )
    )

  override def notice(text: String): F[Unit] =
    emit(obj("type" -> str("notice"), "text" -> str(stripAnsi(text))))

  override def users(list: List[String]): F[Unit] =
    snapshot.update(_.copy(users = list)) *>
      emit(obj("type" -> str("users"), "users" -> arr(list)), remember = false)

  override def typing(list: List[String]): F[Unit] =
    snapshot.update(_.copy(typing = list)) *>
      emit(obj("type" -> str("typing"), "users" -> arr(list)), remember = false)

  override def me(name: String): F[Unit] =
    snapshot.update(_.copy(me = name)) *>
      emit(obj("type" -> str("me"), "name" -> str(name)), remember = false)

  override def bind(send: String => F[Unit]): Resource[F, Unit] =
    Resource.make(sink.set(Some(send)) *> connection(true))(_ =>
      sink.set(None) *> connection(false)
    )

  override def serve: Resource[F, Unit] =
    if !enabled then Resource.unit
    else
      // bind fails on a socket file left behind by a crash, so clear it on both
      // ends of the resource.
      Resource.make(clearSocket)(_ => clearSocket) *>
        Resource.eval(logger.debug(s"IPC socket listening on ${Config.ipcSocketPath}")) *>
        Network[F]
          .bindAndAccept(UnixSocketAddress(Config.ipcSocketPath))
          .map(handle)
          .parJoin(maxClients)
          .compile
          .drain
          .handleErrorWith(err => logger.warn(s"IPC socket unavailable: ${err.getMessage}"))
          .background
          .void

  private def stripAnsi(s: String): String = ansiPattern.replaceAllIn(s, "")

  private def clearSocket: F[Unit] =
    Files[F].deleteIfExists(Path(Config.ipcSocketPath)).void.handleError(_ => ())

  private def connection(connected: Boolean): F[Unit] =
    snapshot.update(_.copy(connected = connected)) *>
      emit(obj("type" -> str("connection"), "connected" -> bool(connected)), remember = false)

  private def emit(json: String, remember: Boolean = true): F[Unit] =
    if !enabled then Applicative[F].unit
    else
      Applicative[F].whenA(remember)(recent.update(v => (v :+ json).takeRight(recentSize))) *>
        topic.publish1(json).void

  private def handle(socket: Socket[F]): Stream[F, Unit] =
    val out =
      Stream
        .resource(topic.subscribeAwait(subscriberQueue))
        .flatMap(events => Stream.evalSeq(hello) ++ events)
        .map(_ + "\n")
        .through(text.utf8.encode)
        .through(socket.writes)

    val in =
      socket.reads
        .through(text.utf8.decode)
        .through(text.lines)
        .map(_.trim)
        .filter(_.nonEmpty)
        .evalMap(feed)

    out.mergeHaltBoth(in).handleErrorWith { err =>
      Stream.exec(logger.debug(s"IPC client dropped: ${err.getMessage}"))
    }

  private def feed(line: String): F[Unit] =
    sink.get.flatMap {
      case Some(send) => send(line)
      case None =>
        emit(
          obj("type" -> str("notice"), "text" -> str("Not connected to the chat server.")),
          remember = false
        )
    }

  private def hello: F[List[String]] =
    (snapshot.get, recent.get).mapN { (snap, buffered) =>
      obj(
        "type" -> str("hello"),
        "protocol" -> num(ipcProtocol),
        "me" -> str(snap.me),
        "users" -> arr(snap.users),
        "typing" -> arr(snap.typing),
        "connected" -> bool(snap.connected)
      ) :: buffered.toList
    }

object LiveIpc:
  def create[F[_]: Async: Network: Files: LoggerFactory]: F[Ipc[F]] =
    for
      topic <- Topic[F, String]
      recent <- Ref.of[F, Vector[String]](Vector.empty)
      snapshot <- Ref.of[F, Ipc.Snapshot](Ipc.Snapshot("", Nil, Nil, connected = false))
      sink <- Ref.of[F, Option[String => F[Unit]]](None)
    yield new LiveIpc[F](Config.ipcEnabled, topic, recent, snapshot, sink)
