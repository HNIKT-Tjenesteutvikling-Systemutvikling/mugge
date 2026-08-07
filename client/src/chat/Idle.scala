package chat

import cats.effect.*
import cats.mtl.Handle.allow
import cats.mtl.Raise
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.io.process.ProcessBuilder
import fs2.io.process.Processes
import fs2.text
import org.typelevel.log4cats.LoggerFactory

import scala.concurrent.duration.*
import scala.sys.process.*

final case class IdleError(message: String)

trait Idle[F[_]]:
  /** Whole-desktop input idleness: `true` when the user went idle, `false` when a key or the mouse
    * brought them back.
    */
  def transitions(after: FiniteDuration): Stream[F, Boolean]

final class LiveIdle[F[_]: Async: Files: Processes: LoggerFactory] private () extends Idle[F]:
  private val logger = LoggerFactory[F].getLogger

  private val pollInterval = 15.seconds
  private val reprobeInterval = 1.minute

  private val mutterIdlePattern = """uint64\s+(\d+)""".r.unanchored

  private val mutterCall = Seq(
    "gdbus",
    "call",
    "--session",
    "--dest",
    "org.gnome.Mutter.IdleMonitor",
    "--object-path",
    "/org/gnome/Mutter/IdleMonitor/Core",
    "--method",
    "org.gnome.Mutter.IdleMonitor.GetIdletime"
  )

  override def transitions(after: FiniteDuration): Stream[F, Boolean] =
    val resolve: F[Stream[F, Boolean]] =
      allow[IdleError](backend(after)).rescue { err =>
        logger.debug(s"No idle backend available: ${err.message}").as(Stream.empty)
      }
    val attempt = Stream.eval(resolve).flatten.handleErrorWith { err =>
      Stream.exec(logger.debug(s"Idle backend stopped: ${err.getMessage}"))
    }
    (attempt ++ Stream.sleep_[F](reprobeInterval)).repeat

  private def orRaise[A](fa: F[A])(using r: Raise[F, IdleError]): F[A] =
    fa.handleErrorWith(e => r.raise(IdleError(Option(e.getMessage).getOrElse(e.toString))))

  private def backend(after: FiniteDuration)(using
      Raise[F, IdleError]
  ): F[Stream[F, Boolean]] =
    mutterIdleMillis.flatMap {
      case Some(_) => mutterTransitions(after).pure[F]
      case None    => swayidleTransitions(after)
    }

  private def mutterTransitions(after: FiniteDuration): Stream[F, Boolean] =
    Stream.eval(Ref.of[F, Boolean](false)).flatMap { wasIdle =>
      Stream
        .awakeEvery[F](pollInterval)
        .evalMap(_ => mutterIdleMillis)
        .unNone
        .evalMapFilter { millis =>
          val idle = millis >= after.toMillis
          wasIdle.getAndSet(idle).map(prev => Option.when(prev != idle)(idle))
        }
    }

  private def mutterIdleMillis: F[Option[Long]] =
    Sync[F]
      .blocking {
        val quiet = ProcessLogger(_ => (), _ => ())
        Process(mutterCall).!!(quiet)
      }
      .attempt
      .map(_.toOption.flatMap {
        case mutterIdlePattern(millis) => millis.toLongOption
        case _                         => None
      })

  private def swayidleTransitions(after: FiniteDuration)(using
      Raise[F, IdleError]
  ): F[Stream[F, Boolean]] =
    waylandEnv.flatMap { env =>
      orRaise(
        ProcessBuilder(
          "swayidle",
          "-w",
          "timeout",
          after.toSeconds.toString,
          "echo idle",
          "resume",
          "echo active"
        ).withExtraEnv(env).spawn[F].allocated
      ).map { case (process, release) =>
        process.stdout
          .through(text.utf8.decode)
          .through(text.lines)
          .map(_.trim)
          .collect {
            case "idle"   => true
            case "active" => false
          }
          .concurrently(process.stderr.drain)
          .onFinalize(release)
      }
    }

  private def waylandEnv: F[Map[String, String]] =
    if sys.env.contains("WAYLAND_DISPLAY") then Map.empty[String, String].pure[F]
    else
      sys.env.get("XDG_RUNTIME_DIR") match
        case None => Map.empty[String, String].pure[F]
        case Some(dir) =>
          Files[F]
            .list(Path(dir))
            .map(_.fileName.toString)
            .filter(name => name.startsWith("wayland-") && !name.endsWith(".lock"))
            .compile
            .toList
            .map(_.sorted.headOption.fold(Map.empty)(d => Map("WAYLAND_DISPLAY" -> d)))
            .handleError(_ => Map.empty)

object LiveIdle:
  def apply[F[_]: Async: Files: Processes: LoggerFactory](): Idle[F] = new LiveIdle[F]()
