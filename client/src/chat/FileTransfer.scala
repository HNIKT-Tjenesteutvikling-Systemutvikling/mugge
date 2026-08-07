package chat

import cats.effect.*
import cats.effect.std.Queue
import cats.effect.std.UUIDGen
import cats.effect.syntax.all.*
import cats.mtl.Handle.allow
import cats.mtl.Raise as MtlRaise
import cats.syntax.all.*
import fs2.io.file.Files as Fs2Files
import fs2.io.file.Path as Fs2Path

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.security.MessageDigest
import java.util.Base64

case class OutgoingFile(path: Path, name: String, size: Long)

case class IncomingFile(
    from: String,
    name: String,
    size: Long,
    temp: Option[Path] = None,
    received: Long = 0
)

private[chat] enum FileError:
  case NotFound(path: String)
  case NotRegular(path: String)
  case NotReadable(path: String)
  case TooLarge(size: Long)
  case Unreadable(detail: String)
  case ChecksumMismatch(name: String)

/** Chunked, base64 file transfer over the chat socket, checksummed end to end. */
trait FileTransfer[F[_]]:
  def prepareSend(
      rest: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit]

  def offer(msg: String, state: Ref[F, ClientState[F]], ui: Ui[F]): F[Unit]

  def accept(
      id: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit]

  def reject(id: String, state: Ref[F, ClientState[F]], ui: Ui[F]): F[Unit]

  def data(msg: String, state: Ref[F, ClientState[F]], ui: Ui[F]): F[Unit]

  def end(msg: String, state: Ref[F, ClientState[F]], ui: Ui[F]): F[Unit]

final class LiveFileTransfer[F[_]: Async: Fs2Files: UUIDGen] private (
    notifications: Notifications[F]
) extends FileTransfer[F]:
  private val maxFileSize = 10L * 1024 * 1024
  private val fileChunkSize = 48 * 1024

  private def fileErrorMessage(e: FileError): String = e match
    case FileError.NotFound(p)         => s"File not found: $p"
    case FileError.NotRegular(p)       => s"Not a regular file: $p"
    case FileError.NotReadable(p)      => s"File is not readable: $p"
    case FileError.TooLarge(s)         => s"File too large ($s bytes). Max is $maxFileSize bytes."
    case FileError.Unreadable(d)       => s"Could not read file: $d"
    case FileError.ChecksumMismatch(n) => s"Checksum mismatch for $n; download discarded."

  private def raiseFile[A](e: FileError)(using r: MtlRaise[F, FileError]): F[A] =
    r.raise(e)

  private def progressMilestone(prev: Long, now: Long, total: Long): Option[Int] =
    if total <= 0 then None
    else
      val p = (prev * 4 / total).toInt
      val n = (now * 4 / total).toInt
      if n > p && n < 4 then Some(n * 25) else None

  override def prepareSend(
      rest: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit] =
    rest.trim.split(" ", 2) match
      case Array(targetToken, rawPath) if targetToken.startsWith("@") =>
        val path = Paths.get(rawPath.trim)
        allow[FileError] {
          for
            checked <- Sync[F].blocking {
              val regular = Files.isRegularFile(path)
              val readable = regular && Files.isReadable(path)
              val size = if regular then Files.size(path) else -1L
              (Files.exists(path), regular, readable, size)
            }.attempt
            size <- checked match
              case Left(err)               => raiseFile(FileError.Unreadable(err.getMessage))
              case Right((false, _, _, _)) => raiseFile(FileError.NotFound(rawPath))
              case Right((_, false, _, _)) => raiseFile(FileError.NotRegular(rawPath))
              case Right((_, _, false, _)) => raiseFile(FileError.NotReadable(rawPath))
              case Right((_, _, _, s)) if s > maxFileSize => raiseFile(FileError.TooLarge(s))
              case Right((_, _, _, s))                    => s.pure[F]
            id <- UUIDGen[F].randomUUID.map(_.toString.take(8))
            name = path.getFileName.toString
            _ <- state.update(st =>
              st.copy(outgoingFiles = st.outgoingFiles + (id -> OutgoingFile(path, name, size)))
            )
            _ <- outgoingQueue.offer(s"/sendfile $targetToken $id $size $name")
          yield ()
        }.rescue(e => ui.printLine(fileErrorMessage(e)))
      case _ =>
        ui.printLine("Usage: /sendfile @user <path>")

  override def offer(
      msg: String,
      state: Ref[F, ClientState[F]],
      ui: Ui[F]
  ): F[Unit] =
    msg.split(":", 5) match
      case Array(_, id, from, sizeStr, name) =>
        val size = sizeStr.toLongOption.getOrElse(0L)
        val safe = sanitizeFilename(name)
        state.update(st =>
          st.copy(incomingFiles = st.incomingFiles + (id -> IncomingFile(from, safe, size)))
        ) *>
          ui.printLine(
            s"$from wants to send \"$safe\" ($size bytes). " +
              s"Accept with /acceptfile $id or decline with /rejectfile $id"
          ) *>
          notifications.send(
            title = s"📎 File offer from $from",
            body = s"$safe ($size bytes) — /acceptfile $id or /rejectfile $id",
            urgency = "critical",
            timeout = 0
          )
      case _ => ().pure[F]

  override def accept(
      id: String,
      state: Ref[F, ClientState[F]],
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit] =
    state
      .modify(st => (st.copy(outgoingFiles = st.outgoingFiles - id), st.outgoingFiles.get(id)))
      .flatMap {
        case Some(out) =>
          ui.printLine(s"Offer accepted; sending ${out.name}...") *>
            sendFileData(id, out, outgoingQueue, ui).start.void
        case None => ().pure[F]
      }

  private def sendFileData(
      id: String,
      out: OutgoingFile,
      outgoingQueue: Queue[F, String],
      ui: Ui[F]
  ): F[Unit] =
    (Sync[F].delay(MessageDigest.getInstance("SHA-256")), Ref.of[F, Long](0L)).flatMapN {
      (md, sent) =>
        Fs2Files[F]
          .readAll(Fs2Path.fromNioPath(out.path))
          .chunkN(fileChunkSize)
          .zipWithIndex
          .evalMap { case (chunk, seq) =>
            val bytes = chunk.toArray
            Sync[F].delay(md.update(bytes)) *>
              outgoingQueue.offer(
                s"FILEDATA:$id:$seq:${Base64.getEncoder.encodeToString(bytes)}"
              ) *>
              sent
                .modify { prev =>
                  val now = prev + bytes.length
                  (now, progressMilestone(prev, now, out.size))
                }
                .flatMap(_.traverse_(pct => ui.printLine(s"Sending ${out.name}... $pct%")))
          }
          .compile
          .drain
          .flatMap(_ => outgoingQueue.offer(s"FILEEND:$id:${toHex(md.digest())}"))
          .flatMap(_ => ui.printLine(s"Sent ${out.name} (${out.size} bytes)."))
          .handleErrorWith(err => ui.printLine(s"Failed to send ${out.name}: ${err.getMessage}"))
    }

  override def reject(
      id: String,
      state: Ref[F, ClientState[F]],
      ui: Ui[F]
  ): F[Unit] =
    state
      .modify(st => (st.copy(outgoingFiles = st.outgoingFiles - id), st.outgoingFiles.get(id)))
      .flatMap {
        case Some(out) => ui.printLine(s"${out.name} was rejected by the recipient.")
        case None      => ().pure[F]
      }

  override def data(msg: String, state: Ref[F, ClientState[F]], ui: Ui[F]): F[Unit] =
    msg.split(":", 4) match
      case Array(_, id, _, b64) =>
        state.get.map(_.incomingFiles.get(id)).flatMap {
          case None => ().pure[F]
          case Some(incoming) =>
            val bytes = Base64.getDecoder.decode(b64)
            val write = incoming.temp match
              case Some(tmp) =>
                Sync[F].blocking(Files.write(tmp, bytes, StandardOpenOption.APPEND)).void
              case None =>
                for
                  tmp <- Sync[F].blocking(Files.createTempFile("mugge-", ".part"))
                  _ <- Sync[F].blocking(Files.write(tmp, bytes, StandardOpenOption.APPEND))
                  _ <- state.update(st =>
                    st.copy(incomingFiles =
                      st.incomingFiles.updatedWith(id)(_.map(_.copy(temp = Some(tmp))))
                    )
                  )
                yield ()
            write *>
              state
                .modify { st =>
                  st.incomingFiles.get(id) match
                    case None => (st, None)
                    case Some(inc) =>
                      val now = inc.received + bytes.length
                      (
                        st.copy(incomingFiles =
                          st.incomingFiles.updated(id, inc.copy(received = now))
                        ),
                        progressMilestone(inc.received, now, inc.size).map((inc, _))
                      )
                }
                .flatMap(_.traverse_ { (inc, pct) =>
                  ui.printLine(s"Receiving ${inc.name} from ${inc.from}... $pct%")
                })
        }
      case _ => ().pure[F]

  override def end(msg: String, state: Ref[F, ClientState[F]], ui: Ui[F]): F[Unit] =
    msg.split(":", 3) match
      case Array(_, id, sha) =>
        state
          .modify(st => (st.copy(incomingFiles = st.incomingFiles - id), st.incomingFiles.get(id)))
          .flatMap {
            case None => ().pure[F]
            case Some(incoming) =>
              for
                tmp <- incoming.temp match
                  case Some(t) => t.pure[F]
                  case None    => Sync[F].blocking(Files.createTempFile("mugge-", ".part"))
                _ <- finalizeIncoming(tmp, sha, incoming, ui)
              yield ()
          }
      case _ => ().pure[F]

  private def finalizeIncoming(
      tmp: Path,
      expectedSha: String,
      incoming: IncomingFile,
      ui: Ui[F]
  ): F[Unit] =
    allow[FileError] {
      streamSha256(tmp).flatMap { actualSha =>
        if !actualSha.equalsIgnoreCase(expectedSha) then
          Sync[F].blocking(Files.deleteIfExists(tmp)).attempt.void *>
            raiseFile(FileError.ChecksumMismatch(incoming.name))
        else
          for
            dir <- downloadDir
            _ <- Sync[F].blocking(Files.createDirectories(dir))
            target <- Sync[F].blocking(uniqueTarget(dir, incoming.name))
            _ <- Sync[F].blocking(Files.move(tmp, target))
            _ <- ui.printLine(s"Saved ${incoming.name} from ${incoming.from} to $target")
            _ <- notifications.send(
              title = s"📎 File received from ${incoming.from}",
              body = s"Saved to $target",
              urgency = "normal"
            )
          yield ()
      }
    }.rescue(e => ui.printLine(fileErrorMessage(e)))

  private def toHex(bytes: Array[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  private def streamSha256(path: Path): F[String] =
    Sync[F].delay(MessageDigest.getInstance("SHA-256")).flatMap { md =>
      Fs2Files[F]
        .readAll(Fs2Path.fromNioPath(path))
        .chunks
        .evalMap(c => Sync[F].delay(md.update(c.toArray)))
        .compile
        .drain
        .map(_ => toHex(md.digest()))
    }

  private def sanitizeFilename(name: String): String =
    val base = name.replace("\\", "/").split("/").filter(_.nonEmpty).lastOption.getOrElse("file")
    val cleaned = base.trim
    if cleaned.isEmpty || cleaned == "." || cleaned == ".." then "file" else cleaned

  private def downloadDir: F[Path] =
    Sync[F].delay {
      sys.env
        .get("XDG_DOWNLOAD_DIR")
        .filter(_.nonEmpty)
        .map(Paths.get(_))
        .getOrElse(Paths.get(System.getProperty("user.home"), "Downloads"))
    }

  private def uniqueTarget(dir: Path, name: String): Path =
    val initial = dir.resolve(name)
    if !Files.exists(initial) then initial
    else
      val dot = name.lastIndexOf('.')
      val (base, ext) =
        if dot > 0 then (name.substring(0, dot), name.substring(dot)) else (name, "")
      LazyList
        .from(1)
        .map(i => dir.resolve(s"$base ($i)$ext"))
        .find(p => !Files.exists(p))
        .getOrElse(initial)

object LiveFileTransfer:
  def apply[F[_]: Async: Fs2Files: UUIDGen](
      notifications: Notifications[F]
  ): FileTransfer[F] = new LiveFileTransfer[F](notifications)
