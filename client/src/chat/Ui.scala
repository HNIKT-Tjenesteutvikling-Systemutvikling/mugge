package chat

import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.Mutex
import cats.syntax.all.*

import Ansi.*

/** Owns the terminal: the chat scrollback, the input block and the online-user side panel, all
  * repainted under a single mutex so concurrent writers never interleave.
  */
final class Ui(
    mutex: Mutex[IO],
    state: Ref[IO, ClientState],
    ictl: InputCtl,
    blockLines: Ref[IO, Int],
    pty: Boolean,
    termSize: Ref[IO, Option[(Int, Int)]],
    scrollback: Ref[IO, Vector[String]]
):
  import Ui.*

  def isTty: Boolean = pty

  private def plainPrint(line: String): IO[Unit] =
    if pty then Console[IO].print(line + "\r\n") else Console[IO].println(line)

  // Stored pre-wrap so a redraw re-wraps at the current width.
  private def appendScrollback(line: String): IO[Unit] =
    scrollback.update(v => (v :+ line).takeRight(scrollbackSize))

  def printLine(line: String): IO[Unit] =
    mutex.lock.surround {
      appendScrollback(line) *>
        termSize.get.flatMap {
          case None               => plainPrint(line)
          case Some((cols, rows)) => render(Some(line), cols, rows)
        }
    }

  def printLines(lines: List[String]): IO[Unit] =
    if lines.isEmpty then IO.unit
    else
      mutex.lock.surround {
        termSize.get.flatMap {
          case None =>
            lines.traverse_(l => appendScrollback(l) *> plainPrint(l))
          case Some((cols, rows)) =>
            lines.traverse_(l => appendScrollback(l) *> render(Some(l), cols, rows))
        }
      }

  def printCodeBlock(
      time: String,
      indicator: String,
      sender: String,
      lang: String,
      code: List[String]
  ): IO[Unit] =
    colorIndexFor(sender, state).flatMap { idx =>
      val header = s"[$time] $indicator ${ansiPalette(idx)}$sender$ansiReset:"
      termSize.get.flatMap {
        case None =>
          val open = if lang.nonEmpty then s"${Markup.fence}$lang" else Markup.fence
          printLines(header :: open :: code ::: List(Markup.fence))
        case Some((cols, _)) =>
          printLines(header :: codeBoxLines(lang, code, math.max(12, cols - panelWidth)))
      }
    }

  private val codeBorderColor = "\u001b[38;5;240m"

  private def codeBoxLines(lang: String, code: List[String], width: Int): List[String] =
    val boxW = math.max(8, width)
    val innerW = boxW - 4
    val label = if lang.nonEmpty then s"─ $lang " else "──"
    val top = s"$codeBorderColor┌$label" + "─" * math.max(0, boxW - 2 - label.length) +
      s"┐$ansiReset"
    val bottom = s"$codeBorderColor└" + "─" * (boxW - 2) + s"┘$ansiReset"
    val rows = code.flatMap { raw =>
      val wrapped = wrapAnsi(Highlighter.highlight(lang, raw), innerW) match
        case Nil  => List("")
        case rows => rows
      wrapped.zipWithIndex.map { (row, idx) =>
        val gutter = if idx == 0 then " " else s"\u001b[2m↳$ansiReset"
        s"$codeBorderColor│$ansiReset$gutter${padVisible(row, innerW)} " +
          s"$codeBorderColor│$ansiReset"
      }
    }
    top :: rows ::: List(bottom)

  def setUsers(users: List[String]): IO[Unit] =
    state
      .modify { st =>
        val pruned = st.statuses.filter((name, _) => users.contains(name))
        val changed = st.onlineUsers != users || st.statuses != pruned
        (st.copy(onlineUsers = users, statuses = pruned), changed)
      }
      .flatMap { changed =>
        if !changed then IO.unit
        else
          mutex.lock.surround {
            termSize.get.flatMap {
              case None if pty        => IO.unit
              case None               => plainPrint(s"Online: ${users.mkString(", ")}")
              case Some((cols, rows)) => render(None, cols, rows)
            }
          }
      }

  def setStatuses(name: String, status: Option[String]): IO[Unit] =
    state
      .modify { st =>
        val updated = status.fold(st.statuses - name)(s => st.statuses + (name -> s))
        (st.copy(statuses = updated), st.statuses != updated)
      }
      .flatMap { changed =>
        if !changed then IO.unit
        else
          mutex.lock.surround {
            termSize.get.flatMap {
              case None               => IO.unit
              case Some((cols, rows)) => render(None, cols, rows)
            }
          }
      }

  def setVoiceUsers(users: List[String]): IO[Unit] =
    state.modify(st => (st.copy(voiceUsers = users), st.voiceUsers != users)).flatMap { changed =>
      if !changed then IO.unit
      else
        mutex.lock.surround {
          termSize.get.flatMap {
            case None               => IO.unit
            case Some((cols, rows)) => render(None, cols, rows)
          }
        }
    }

  def setTyping(users: List[String]): IO[Unit] =
    state.modify(st => (st.copy(typingUsers = users), st.typingUsers != users)).flatMap { changed =>
      if !changed then IO.unit
      else
        mutex.lock.surround {
          termSize.get.flatMap {
            case None               => IO.unit
            case Some((cols, rows)) => render(None, cols, rows)
          }
        }
    }

  def refreshInput: IO[Unit] =
    mutex.lock.surround {
      termSize.get.flatMap {
        case None               => IO.unit
        case Some((cols, rows)) => render(None, cols, rows)
      }
    }

  // Re-detect the size first so it repaints straight out of the headless
  // (None) state on the first attach, before the watcher poll lands.
  def redraw: IO[Unit] =
    Terminal.size.flatMap { latest =>
      mutex.lock.surround {
        termSize.set(latest) *> (latest match
          case None               => IO.unit
          case Some((cols, rows)) => fullRepaint(cols, rows)
        )
      }
    }

  private def fullRepaint(cols: Int, rows: Int): IO[Unit] =
    for
      st <- state.get
      inp <- ictl.text.get
      paste <- ictl.pendingPaste.get
      hint <- ictl.hint.get
      ring <- scrollback.get
      startCol = math.max(1, cols - panelWidth + 1)
      textWidth = math.max(1, startCol - 1)
      clearRows = math.min(rows - 1, 30)
      visible = st.onlineUsers.take(math.max(0, clearRows - 1))
      colored <- visible.traverse(u =>
        colorIndexFor(u, state).map(idx =>
          (u, ansiPalette(idx), ansiDimPalette(idx), st.statuses.get(u).filter(_.nonEmpty))
        )
      )
      (blockStr, newCount) = renderBlock(st, inp, paste, hint, textWidth, rows)
      sb = new StringBuilder
      _ = sb.append("\u001b[2J\u001b[H")
      _ = ring.foreach(l => wrapAnsi(l, textWidth).foreach(r => sb.append(r).append("\n")))
      _ = sb.append(blockStr)
      _ = sb.append(
        panelStr(colored, st.voiceUsers.toSet, st.onlineUsers.size, startCol, clearRows)
      )
      _ <- Console[IO].print(sb.toString)
      _ <- blockLines.set(newCount)
    yield ()

  private def render(chat: Option[String], cols: Int, rows: Int): IO[Unit] =
    for
      st <- state.get
      inp <- ictl.text.get
      paste <- ictl.pendingPaste.get
      hint <- ictl.hint.get
      prev <- blockLines.get
      startCol = math.max(1, cols - panelWidth + 1)
      textWidth = math.max(1, startCol - 1)
      clearRows = math.min(rows - 1, 30)
      visible = st.onlineUsers.take(math.max(0, clearRows - 1))
      colored <- visible.traverse(u =>
        colorIndexFor(u, state).map(idx =>
          (u, ansiPalette(idx), ansiDimPalette(idx), st.statuses.get(u).filter(_.nonEmpty))
        )
      )
      (blockStr, newCount) = renderBlock(st, inp, paste, hint, textWidth, rows)
      sb = new StringBuilder
      _ = sb.append(eraseBlock(prev))
      _ = chat.foreach(l => wrapAnsi(l, textWidth).foreach(r => sb.append(r).append("\n")))
      _ = sb.append(blockStr)
      _ = sb.append(
        panelStr(colored, st.voiceUsers.toSet, st.onlineUsers.size, startCol, clearRows)
      )
      _ <- Console[IO].print(sb.toString)
      _ <- blockLines.set(newCount)
    yield ()

  private def eraseBlock(n: Int): String =
    if n <= 0 then ""
    else
      val up = if n > 1 then s"\u001b[${n - 1}A" else ""
      s"\r$up\u001b[0J"

  private def renderBlock(
      st: ClientState,
      inp: String,
      paste: Option[PendingPaste],
      hint: Option[String],
      width: Int,
      rows: Int
  ): (String, Int) =
    def dim(s: String) = s"\u001b[2m${wrapAnsi(s, width).head}$ansiReset"
    val hintRow = hint.map(dim)
    val typingRow = formatTyping(st.typingUsers).map(dim)
    val pasteTag =
      paste.fold("") { p =>
        val lines = p.text.count(_ == '\n') + 1
        p.codeLang match
          case Some(_) => s"\u001b[2m[code: ${math.max(0, lines - 2)} lines]$ansiReset"
          case None    => s"\u001b[2m[paste: $lines lines]$ansiReset"
      }
    // Split on typed newlines (Alt/Shift+Enter), wrap each, tail-follow.
    val inputRows =
      (inputPrompt + inp + pasteTag)
        .split("\n", -1)
        .toList
        .flatMap(seg =>
          wrapAnsi(seg, width) match
            case Nil => List("")
            case rs  => rs
        )
        .takeRight(math.max(1, rows - 2))
    val all = hintRow.toList ++ typingRow.toList ++ inputRows
    (all.map(r => s"\r\u001b[2K$r").mkString("\n"), all.size)

  private def panelStr(
      colored: List[(String, String, String, Option[String])],
      voice: Set[String],
      total: Int,
      startCol: Int,
      clearRows: Int
  ): String =
    val clears =
      (1 to clearRows).map(r => s"\u001b[$r;${startCol}H" + " " * panelWidth).mkString
    val header = s"\u001b[1;${startCol}H\u001b[1m\u2524 Online ($total)\u001b[0m"
    val entries = colored.zipWithIndex.map { case ((u, color, dim, status), i) =>
      val bullet = if voice.contains(u) then "\u266a" else "\u2022"
      s"\u001b[${2 + i};${startCol}H$color$bullet ${panelLabel(u, status, color, dim)}"
    }.mkString
    s"\u001b7$clears$header$entries\u001b8"

  private def panelLabel(
      name: String,
      status: Option[String],
      bright: String,
      dim: String
  ): String =
    val avail = math.max(1, panelWidth - 2)
    status match
      case Some(s) if name.length <= avail - 4 =>
        val room = avail - name.length - 3
        val shown = if s.length > room then s.take(room - 1) + "\u2026" else s
        s"$bright$name$ansiReset$dim ($shown)$ansiReset"
      case _ =>
        s"$bright${if name.length > avail then name.take(avail) else name}$ansiReset"

object Ui:
  private val panelWidth = 24

  private val inputPrompt = "> "

  // dtach keeps no scrollback, so the client keeps its own to repaint on attach.
  private val scrollbackSize = 200

  private def formatTyping(users: List[String]): Option[String] =
    users match
      case Nil      => None
      case a :: Nil => Some(s"$a is typing...")
      case _ =>
        val init = users.init.mkString(", ")
        Some(s"$init and ${users.last} is typing...")

  private def colorIndexFor(name: String, state: Ref[IO, ClientState]): IO[Int] =
    state.modify { st =>
      st.colors.get(name) match
        case Some(idx) => (st, idx)
        case None =>
          val idx = st.colors.size % ansiPalette.size
          (st.copy(colors = st.colors + (name -> idx)), idx)
    }

  def colorize(
      msg: String,
      state: Ref[IO, ClientState],
      withStatus: Boolean = true
  ): IO[String] =
    msg match
      case Markup.displayPattern(time, indicator, sender, content) =>
        if sender.trim == "SERVER" then
          IO.pure(
            s"[$time] $indicator $serverColor$sender$ansiReset: " +
              s"$serverColor${linkify(content)}$ansiReset"
          )
        else
          (colorIndexFor(sender.trim, state), state.get).flatMapN { (idx, st) =>
            val bright = ansiPalette(idx)
            val dim = ansiDimPalette(idx)
            val status =
              if withStatus then st.statuses.get(sender.trim).filter(_.nonEmpty) else None
            val statusPart = status.fold("")(s => s" $dim($s)$ansiReset")
            IO.pure(
              s"[$time] $indicator $bright$sender$ansiReset$statusPart: " +
                s"$dim${linkify(content)}$ansiReset"
            )
          }
      case _ => IO.pure(msg)
