package chat

import cats.effect.*
import cats.mtl.Handle.allow
import cats.mtl.Raise
import fs2.Stream
import fs2.io.file.Files as Fs2Files
import fs2.io.file.Path as Fs2Path
import fs2.io.process.ProcessBuilder
import fs2.text
import org.typelevel.log4cats.Logger as TLogger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*
import scala.sys.process.*

object Idle:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  final case class IdleError(message: String)

  private def orRaise[A](io: IO[A])(using r: Raise[IO, IdleError]): IO[A] =
    io.handleErrorWith(e => r.raise(IdleError(Option(e.getMessage).getOrElse(e.toString))))

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

  /** Whole-desktop input idleness: `true` when the user went idle, `false` when a key or the mouse
    * brought them back.
    */
  def transitions(after: FiniteDuration): Stream[IO, Boolean] =
    val resolve: IO[Stream[IO, Boolean]] =
      allow[IdleError](backend(after)).rescue { err =>
        logger.debug(s"No idle backend available: ${err.message}").as(Stream.empty)
      }
    // Re-resolved on every stop: the always-on service starts before the
    // compositor, so the first probe can come up empty, and a compositor
    // restart takes swayidle with it.
    val attempt = Stream.eval(resolve).flatten.handleErrorWith { err =>
      Stream.exec(logger.debug(s"Idle backend stopped: ${err.getMessage}"))
    }
    (attempt ++ Stream.sleep_[IO](reprobeInterval)).repeat

  private def backend(after: FiniteDuration)(using
      Raise[IO, IdleError]
  ): IO[Stream[IO, Boolean]] =
    mutterIdleMillis.flatMap {
      case Some(_) => IO.pure(mutterTransitions(after))
      case None    => swayidleTransitions(after)
    }

  // Mutter reports idle time but offers no event to await, so GNOME is polled;
  // every other compositor is subscribed to through swayidle.
  private def mutterTransitions(after: FiniteDuration): Stream[IO, Boolean] =
    Stream.eval(Ref.of[IO, Boolean](false)).flatMap { wasIdle =>
      Stream
        .awakeEvery[IO](pollInterval)
        .evalMap(_ => mutterIdleMillis)
        .unNone
        .evalMapFilter { millis =>
          val idle = millis >= after.toMillis
          wasIdle.getAndSet(idle).map(prev => Option.when(prev != idle)(idle))
        }
    }

  private def mutterIdleMillis: IO[Option[Long]] =
    IO.blocking {
      val quiet = ProcessLogger(_ => (), _ => ())
      Process(mutterCall).!!(quiet)
    }.attempt
      .map(_.toOption.flatMap {
        case mutterIdlePattern(millis) => millis.toLongOption
        case _                         => None
      })

  private def swayidleTransitions(after: FiniteDuration)(using
      Raise[IO, IdleError]
  ): IO[Stream[IO, Boolean]] =
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
        ).withExtraEnv(env).spawn[IO].allocated
      ).map { case (process, release) =>
        process.stdout
          .through(text.utf8.decode)
          .through(text.lines)
          .map(_.trim)
          .collect {
            case "idle"   => true
            case "active" => false
          }
          // fs2 always pipes stderr; left undrained, swayidle's logging would
          // eventually block on a full pipe.
          .concurrently(process.stderr.drain)
          .onFinalize(release)
      }
    }

  // The unit's environment snapshot predates the graphical session, so
  // WAYLAND_DISPLAY is usually missing: resolve the socket from the runtime dir.
  private def waylandEnv: IO[Map[String, String]] =
    if sys.env.contains("WAYLAND_DISPLAY") then IO.pure(Map.empty)
    else
      sys.env.get("XDG_RUNTIME_DIR") match
        case None => IO.pure(Map.empty)
        case Some(dir) =>
          Fs2Files[IO]
            .list(Fs2Path(dir))
            .map(_.fileName.toString)
            .filter(name => name.startsWith("wayland-") && !name.endsWith(".lock"))
            .compile
            .toList
            .map(_.sorted.headOption.fold(Map.empty)(d => Map("WAYLAND_DISPLAY" -> d)))
            .handleError(_ => Map.empty)
