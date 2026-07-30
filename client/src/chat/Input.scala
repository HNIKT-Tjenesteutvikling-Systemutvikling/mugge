package chat

import cats.effect.*
import fs2.Stream

/** `pos` is None while the live draft is on the prompt, Some(i) while browsing; `draft` holds the
  * text browsing started from, restored on stepping past the end.
  */
final case class InputHistory(entries: Vector[String], pos: Option[Int], draft: String)

object InputHistory:
  val empty: InputHistory = InputHistory(Vector.empty, None, "")

  private val historySize = 200
  private val maxHistoryEntryChars = 4 * 1024

  def record(h: InputHistory, line: String): InputHistory =
    val keep =
      line.trim.nonEmpty &&
        line.length <= maxHistoryEntryChars &&
        !h.entries.lastOption.contains(line)
    if keep then InputHistory((h.entries :+ line).takeRight(historySize), None, "")
    else h.copy(pos = None, draft = "")

  def prev(h: InputHistory, current: String): (InputHistory, Option[String]) =
    h.pos match
      case _ if h.entries.isEmpty => (h, None)
      case None =>
        val i = h.entries.size - 1
        (h.copy(pos = Some(i), draft = current), Some(h.entries(i)))
      case Some(0) => (h, None)
      case Some(i) => (h.copy(pos = Some(i - 1)), Some(h.entries(i - 1)))

  def next(h: InputHistory): (InputHistory, Option[String]) =
    h.pos match
      case None => (h, None)
      case Some(i) if i >= h.entries.size - 1 =>
        (h.copy(pos = None, draft = ""), Some(h.draft))
      case Some(i) => (h.copy(pos = Some(i + 1)), Some(h.entries(i + 1)))

final case class InputCtl(
    text: Ref[IO, String],
    hint: Ref[IO, Option[String]],
    pendingPaste: Ref[IO, Option[PendingPaste]],
    paste: Ref[IO, Option[(List[Char], Int)]],
    composing: Ref[IO, Boolean],
    history: Ref[IO, InputHistory]
)

enum InputToken:
  case Ch(c: Char)
  case PasteStart, PasteEnd, Newline, HistoryPrev, HistoryNext

object InputToken:
  private enum EscState:
    case Ground, Esc, Ss3
    case Csi(params: String)

  // Kitty keyboard protocol: a modified Enter (e.g. Shift+Enter -> ESC[13;2u)
  // inserts a newline; an unmodified one still submits.
  private def csiUToken(params: String): List[InputToken] =
    params.split(";").toList match
      case key :: rest if key == "13" || key == "10" =>
        val mod = rest.headOption.map(_.takeWhile(_.isDigit)).flatMap(_.toIntOption).getOrElse(1)
        if mod > 1 then List(InputToken.Newline) else List(InputToken.Ch('\r'))
      case _ => Nil

  def tokenize(chars: Stream[IO, Char]): Stream[IO, InputToken] =
    chars
      .mapAccumulate(EscState.Ground: EscState) { (st, c) =>
        st match
          case EscState.Ground =>
            if c == '\u001b' then (EscState.Esc, Nil)
            else (EscState.Ground, List(InputToken.Ch(c)))
          case EscState.Esc =>
            if c == '[' then (EscState.Csi(""), Nil)
            // SS3: in application-cursor-key mode an arrow arrives as ESC O A.
            else if c == 'O' then (EscState.Ss3, Nil)
            else if c == '\r' || c == '\n' then (EscState.Ground, List(InputToken.Newline))
            else (EscState.Ground, Nil)
          case EscState.Ss3 =>
            c match
              case 'A' => (EscState.Ground, List(InputToken.HistoryPrev))
              case 'B' => (EscState.Ground, List(InputToken.HistoryNext))
              case _   => (EscState.Ground, Nil)
          case EscState.Csi(params) =>
            if c >= '@' && c <= '~' then
              val tok = (params, c) match
                case ("200", '~') => List(InputToken.PasteStart)
                case ("201", '~') => List(InputToken.PasteEnd)
                case (p, 'u')     => csiUToken(p)
                case (_, 'A')     => List(InputToken.HistoryPrev)
                case (_, 'B')     => List(InputToken.HistoryNext)
                case _            => Nil
              (EscState.Ground, tok)
            else (EscState.Csi(params + c), Nil)
      }
      .flatMap((_, toks) => Stream.emits(toks))
