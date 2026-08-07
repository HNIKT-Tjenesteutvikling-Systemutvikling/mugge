package chat

import cats.effect.*
import cats.effect.std.Console
import cats.syntax.all.*

import scala.sys.process.*

trait Terminal[F[_]]:
  def size: F[Option[(Int, Int)]]
  def rawMode(pty: Boolean): Resource[F, Unit]

object Terminal:
  // Bracketed paste + the Kitty keyboard protocol (flag 1, "disambiguate"), so
  // Shift+Enter arrives as ESC[13;2u instead of a bare CR we can't tell apart.
  val enableInputModes = "\u001b[?2004h\u001b[>1u"
  private[chat] val disableInputModes = "\u001b[?2004l\u001b[<u"

final class LiveTerminal[F[_]: Sync: Console] private () extends Terminal[F]:
  import Terminal.disableInputModes
  import Terminal.enableInputModes

  override def size: F[Option[(Int, Int)]] =
    Sync[F].delay(Option(System.console())).flatMap {
      case None => none[(Int, Int)].pure[F]
      case Some(_) =>
        Sync[F]
          .blocking(Seq("sh", "-c", "stty size < /dev/tty").!!.trim)
          .map { out =>
            out.split("\\s+").toList match
              case rows :: cols :: Nil =>
                (cols.toIntOption, rows.toIntOption).tupled.filter((c, r) => c > 0 && r > 0)
              case _ => None
          }
          .handleError(_ => None)
    }

  override def rawMode(pty: Boolean): Resource[F, Unit] =
    if !pty then Resource.unit[F]
    else
      Resource
        .make(
          Sync[F]
            .blocking(Seq("sh", "-c", "stty -g < /dev/tty").!!.trim)
            .flatTap(_ =>
              Sync[F]
                .blocking(Seq("sh", "-c", "stty -icanon -echo min 1 time 0 < /dev/tty").!)
                .void
            )
            .flatTap(_ => Console[F].print(enableInputModes))
            .handleError(_ => "")
        )(saved =>
          Console[F].print(disableInputModes).attempt.void *>
            (if saved.isEmpty then ().pure[F]
             else Sync[F].blocking(Seq("sh", "-c", s"stty $saved < /dev/tty").!).attempt.void)
        )
        .void

object LiveTerminal:
  def apply[F[_]: Sync: Console](): Terminal[F] = new LiveTerminal[F]()
