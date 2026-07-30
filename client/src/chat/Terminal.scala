package chat

import cats.effect.*
import cats.effect.std.Console
import cats.syntax.all.*

import scala.sys.process.*

object Terminal:
  // Bracketed paste + the Kitty keyboard protocol (flag 1, "disambiguate"), so
  // Shift+Enter arrives as ESC[13;2u instead of a bare CR we can't tell apart.
  val enableInputModes = "\u001b[?2004h\u001b[>1u"
  private val disableInputModes = "\u001b[?2004l\u001b[<u"

  def size: IO[Option[(Int, Int)]] =
    IO(Option(System.console())).flatMap {
      case None => IO.pure(None)
      case Some(_) =>
        IO.blocking(Seq("sh", "-c", "stty size < /dev/tty").!!.trim)
          .map { out =>
            out.split("\\s+").toList match
              case rows :: cols :: Nil =>
                (cols.toIntOption, rows.toIntOption).tupled.filter((c, r) => c > 0 && r > 0)
              case _ => None
          }
          .handleError(_ => None)
    }

  def rawMode(pty: Boolean): Resource[IO, Unit] =
    if !pty then Resource.unit[IO]
    else
      Resource
        .make(
          IO.blocking(Seq("sh", "-c", "stty -g < /dev/tty").!!.trim)
            .flatTap(_ =>
              IO.blocking(Seq("sh", "-c", "stty -icanon -echo min 1 time 0 < /dev/tty").!).void
            )
            .flatTap(_ => Console[IO].print(enableInputModes))
            .handleError(_ => "")
        )(saved =>
          Console[IO].print(disableInputModes).attempt.void *>
            (if saved.isEmpty then IO.unit
             else IO.blocking(Seq("sh", "-c", s"stty $saved < /dev/tty").!).attempt.void)
        )
        .void
