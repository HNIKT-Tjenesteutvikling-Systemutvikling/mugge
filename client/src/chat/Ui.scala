package chat

import cats.Applicative
import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.Mutex
import cats.syntax.all.*

import Ansi.*

/** Owns the terminal: the chat scrollback, the input block and the online-user side panel, all
  * repainted under a single mutex so concurrent writers never interleave.
  */
trait Ui[F[_]]:
  def isTty: Boolean
  def printLine(line: String): F[Unit]
  def printLines(lines: List[String]): F[Unit]
  def printCodeBlock(
      time: String,
      indicator: String,
      sender: String,
      lang: String,
      code: List[String]
  ): F[Unit]
  def setUsers(users: List[String]): F[Unit]
  def setColors(colors: Map[String, ColorSpec]): F[Unit]
  def setStatuses(name: String, status: Option[String]): F[Unit]
  def setVoiceUsers(users: List[String]): F[Unit]
  def setTyping(users: List[String]): F[Unit]
  def refreshInput: F[Unit]
  def redraw: F[Unit]
  def colorize(msg: String, state: Ref[F, ClientState[F]], withStatus: Boolean = true): F[String]

final class LiveUi[F[_]: Async: Console] private (
    mutex: Mutex[F],
    state: Ref[F, ClientState[F]],
    ictl: InputCtl[F],
    blockLines: Ref[F, Int],
    pty: Boolean,
    termSize: Ref[F, Option[(Int, Int)]],
    scrollback: Ref[F, Vector[String]],
    ansi: Ansi,
    highlighter: Highlighter,
    terminal: Terminal[F]
) extends Ui[F]:
  private val panelWidth = 24

  private val inputPrompt = "> "

  private val scrollbackSize = 200

  private val codeBorderColor = "\u001b[38;5;240m"

  override def isTty: Boolean = pty

  override def printLine(line: String): F[Unit] =
    mutex.lock.surround {
      appendScrollback(line) *>
        termSize.get.flatMap {
          case None               => plainPrint(line)
          case Some((cols, rows)) => render(Some(line), cols, rows)
        }
    }

  override def printLines(lines: List[String]): F[Unit] =
    if lines.isEmpty then Applicative[F].unit
    else
      mutex.lock.surround {
        termSize.get.flatMap {
          case None =>
            lines.traverse_(l => appendScrollback(l) *> plainPrint(l))
          case Some((cols, rows)) =>
            lines.traverse_(l => appendScrollback(l) *> render(Some(l), cols, rows))
        }
      }

  override def printCodeBlock(
      time: String,
      indicator: String,
      sender: String,
      lang: String,
      code: List[String]
  ): F[Unit] =
    specFor(sender, state).flatMap { spec =>
      val header = s"[$time] $indicator ${paintName(sender, spec, shimmer(time))}:"
      termSize.get.flatMap {
        case None =>
          val open = if lang.nonEmpty then s"${Markup.fence}$lang" else Markup.fence
          printLines(header :: open :: code ::: List(Markup.fence))
        case Some((cols, _)) =>
          printLines(header :: codeBoxLines(lang, code, math.max(12, cols - panelWidth)))
      }
    }

  override def setUsers(users: List[String]): F[Unit] =
    state
      .modify { st =>
        val pruned = st.statuses.filter((name, _) => users.contains(name))
        val changed = st.onlineUsers != users || st.statuses != pruned
        (st.copy(onlineUsers = users, statuses = pruned), changed)
      }
      .flatMap { changed =>
        if !changed then Applicative[F].unit
        else
          mutex.lock.surround {
            termSize.get.flatMap {
              case None if pty        => Applicative[F].unit
              case None               => plainPrint(s"Online: ${users.mkString(", ")}")
              case Some((cols, rows)) => render(None, cols, rows)
            }
          }
      }

  override def setColors(colors: Map[String, ColorSpec]): F[Unit] =
    state.modify(st => (st.copy(serverColors = colors), st.serverColors != colors)).flatMap {
      changed =>
        if !changed then Applicative[F].unit
        else
          mutex.lock.surround {
            termSize.get.flatMap {
              case None               => Applicative[F].unit
              case Some((cols, rows)) => render(None, cols, rows)
            }
          }
    }

  override def setStatuses(name: String, status: Option[String]): F[Unit] =
    state
      .modify { st =>
        val updated = status.fold(st.statuses - name)(s => st.statuses + (name -> s))
        (st.copy(statuses = updated), st.statuses != updated)
      }
      .flatMap { changed =>
        if !changed then Applicative[F].unit
        else
          mutex.lock.surround {
            termSize.get.flatMap {
              case None               => Applicative[F].unit
              case Some((cols, rows)) => render(None, cols, rows)
            }
          }
      }

  override def setVoiceUsers(users: List[String]): F[Unit] =
    state.modify(st => (st.copy(voiceUsers = users), st.voiceUsers != users)).flatMap { changed =>
      if !changed then Applicative[F].unit
      else
        mutex.lock.surround {
          termSize.get.flatMap {
            case None               => Applicative[F].unit
            case Some((cols, rows)) => render(None, cols, rows)
          }
        }
    }

  override def setTyping(users: List[String]): F[Unit] =
    state.modify(st => (st.copy(typingUsers = users), st.typingUsers != users)).flatMap { changed =>
      if !changed then Applicative[F].unit
      else
        mutex.lock.surround {
          termSize.get.flatMap {
            case None               => Applicative[F].unit
            case Some((cols, rows)) => render(None, cols, rows)
          }
        }
    }

  override def refreshInput: F[Unit] =
    mutex.lock.surround {
      termSize.get.flatMap {
        case None               => Applicative[F].unit
        case Some((cols, rows)) => render(None, cols, rows)
      }
    }

  override def redraw: F[Unit] =
    terminal.size.flatMap { latest =>
      mutex.lock.surround {
        termSize.set(latest) *> (latest match
          case None               => Applicative[F].unit
          case Some((cols, rows)) => fullRepaint(cols, rows)
        )
      }
    }

  override def colorize(
      msg: String,
      state: Ref[F, ClientState[F]],
      withStatus: Boolean = true
  ): F[String] =
    msg match
      case Markup.displayPattern(time, indicator, sender, content) =>
        if sender.trim == "SERVER" then
          (s"[$time] $indicator $serverColor$sender$ansiReset: " +
            s"$serverColor${ansi.linkify(content)}$ansiReset").pure[F]
        else
          (specFor(sender.trim, state), state.get).flatMapN { (spec, st) =>
            val offset = shimmer(time)
            val dim = dimOf(spec, offset)
            val status =
              if withStatus then st.statuses.get(sender.trim).filter(_.nonEmpty) else None
            val statusPart = status.fold("")(s => s" $dim($s)$ansiReset")
            (s"[$time] $indicator ${paintName(sender, spec, offset)}$statusPart: " +
              s"$dim${ansi.linkify(content)}$ansiReset").pure[F]
          }
      case _ => msg.pure[F]

  private def plainPrint(line: String): F[Unit] =
    if pty then Console[F].print(line + "\r\n") else Console[F].println(line)

  private def appendScrollback(line: String): F[Unit] =
    scrollback.update(v => (v :+ line).takeRight(scrollbackSize))

  private def codeBoxLines(lang: String, code: List[String], width: Int): List[String] =
    val boxW = math.max(8, width)
    val innerW = boxW - 4
    val label = if lang.nonEmpty then s"─ $lang " else "──"
    val top = s"$codeBorderColor┌$label" + "─" * math.max(0, boxW - 2 - label.length) +
      s"┐$ansiReset"
    val bottom = s"$codeBorderColor└" + "─" * (boxW - 2) + s"┘$ansiReset"
    val rows = code.flatMap { raw =>
      val wrapped = ansi.wrapAnsi(highlighter.highlight(lang, raw), innerW) match
        case Nil  => List("")
        case rows => rows
      wrapped.zipWithIndex.map { (row, idx) =>
        val gutter = if idx == 0 then " " else s"\u001b[2m↳$ansiReset"
        s"$codeBorderColor│$ansiReset$gutter${ansi.padVisible(row, innerW)} " +
          s"$codeBorderColor│$ansiReset"
      }
    }
    top :: rows ::: List(bottom)

  private def fullRepaint(cols: Int, rows: Int): F[Unit] =
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
        specFor(u, state).map(spec => (u, spec, st.statuses.get(u).filter(_.nonEmpty)))
      )
      (blockStr, newCount) = renderBlock(st, inp, paste, hint, textWidth, rows)
      sb = new StringBuilder
      _ = sb.append("\u001b[2J\u001b[H")
      _ = ring.foreach(l => ansi.wrapAnsi(l, textWidth).foreach(r => sb.append(r).append("\n")))
      _ = sb.append(blockStr)
      _ = sb.append(
        panelStr(colored, st.voiceUsers.toSet, st.onlineUsers.size, startCol, clearRows)
      )
      _ <- Console[F].print(sb.toString)
      _ <- blockLines.set(newCount)
    yield ()

  private def render(chat: Option[String], cols: Int, rows: Int): F[Unit] =
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
        specFor(u, state).map(spec => (u, spec, st.statuses.get(u).filter(_.nonEmpty)))
      )
      (blockStr, newCount) = renderBlock(st, inp, paste, hint, textWidth, rows)
      sb = new StringBuilder
      _ = sb.append(eraseBlock(prev))
      _ = chat.foreach(l => ansi.wrapAnsi(l, textWidth).foreach(r => sb.append(r).append("\n")))
      _ = sb.append(blockStr)
      _ = sb.append(
        panelStr(colored, st.voiceUsers.toSet, st.onlineUsers.size, startCol, clearRows)
      )
      _ <- Console[F].print(sb.toString)
      _ <- blockLines.set(newCount)
    yield ()

  private def eraseBlock(n: Int): String =
    if n <= 0 then ""
    else
      val up = if n > 1 then s"\u001b[${n - 1}A" else ""
      s"\r$up\u001b[0J"

  private def renderBlock(
      st: ClientState[F],
      inp: String,
      paste: Option[PendingPaste],
      hint: Option[String],
      width: Int,
      rows: Int
  ): (String, Int) =
    def dim(s: String) = s"\u001b[2m${ansi.wrapAnsi(s, width).head}$ansiReset"
    val hintRow = hint.map(dim)
    val typingRow = formatTyping(st.typingUsers).map(dim)
    val pasteTag =
      paste.fold("") { p =>
        val lines = p.text.count(_ == '\n') + 1
        p.codeLang match
          case Some(_) => s"\u001b[2m[code: ${math.max(0, lines - 2)} lines]$ansiReset"
          case None    => s"\u001b[2m[paste: $lines lines]$ansiReset"
      }
    val inputRows =
      (inputPrompt + inp + pasteTag)
        .split("\n", -1)
        .toList
        .flatMap(seg =>
          ansi.wrapAnsi(seg, width) match
            case Nil => List("")
            case rs  => rs
        )
        .takeRight(math.max(1, rows - 2))
    val all = hintRow.toList ++ typingRow.toList ++ inputRows
    (all.map(r => s"\r\u001b[2K$r").mkString("\n"), all.size)

  private def panelStr(
      colored: List[(String, ColorSpec, Option[String])],
      voice: Set[String],
      total: Int,
      startCol: Int,
      clearRows: Int
  ): String =
    val clears =
      (1 to clearRows).map(r => s"\u001b[$r;${startCol}H" + " " * panelWidth).mkString
    val header = s"\u001b[1;${startCol}H\u001b[1m\u2524 Online ($total)\u001b[0m"
    val entries = colored.zipWithIndex.map { case ((u, spec, status), i) =>
      val bullet = if voice.contains(u) then "\u266a" else "\u2022"
      s"\u001b[${2 + i};${startCol}H${brightOf(spec, 0)}$bullet ${panelLabel(u, status, spec)}"
    }.mkString
    s"\u001b7$clears$header$entries\u001b8"

  private def panelLabel(name: String, status: Option[String], spec: ColorSpec): String =
    val avail = math.max(1, panelWidth - 2)
    status match
      case Some(s) if name.length <= avail - 4 =>
        val room = avail - name.length - 3
        val shown = if s.length > room then s.take(room - 1) + "\u2026" else s
        s"${paintName(name, spec, 0)}${dimOf(spec, 0)} ($shown)$ansiReset"
      case _ =>
        paintName(if name.length > avail then name.take(avail) else name, spec, 0)

  private def formatTyping(users: List[String]): Option[String] =
    users match
      case Nil      => None
      case a :: Nil => Some(s"$a is typing...")
      case _ =>
        val init = users.init.mkString(", ")
        Some(s"$init and ${users.last} is typing...")

  /** The server assigns colours so all clients agree; names it did not send (history from users who
    * have since left) fall back to a locally assigned palette slot.
    */
  private def specFor(name: String, state: Ref[F, ClientState[F]]): F[ColorSpec] =
    state.modify { st =>
      st.serverColors.get(name) match
        case Some(spec) => (st, spec)
        case None =>
          st.colors.get(name) match
            case Some(idx) => (st, ColorSpec.Palette(idx))
            case None =>
              val live = (st.onlineUsers.toSet - name).flatMap(u =>
                st.serverColors
                  .get(u)
                  .collect { case ColorSpec.Palette(i) => i }
                  .orElse(st.colors.get(u))
              )
              val idx = ansiPalette.indices
                .find(!live.contains(_))
                .getOrElse(st.colors.size % ansiPalette.size)
              (st.copy(colors = st.colors + (name -> idx)), ColorSpec.Palette(idx))
    }

  private def paintName(name: String, spec: ColorSpec, offset: Int): String =
    spec match
      case ColorSpec.Palette(i) => s"${ansiPalette(i)}$name$ansiReset"
      case ColorSpec.Rainbow    => Ansi.rainbow(name, offset)

  private def brightOf(spec: ColorSpec, offset: Int): String =
    spec match
      case ColorSpec.Palette(i) => ansiPalette(i)
      case ColorSpec.Rainbow    => rainbowPalette(offset % rainbowPalette.size)

  private def dimOf(spec: ColorSpec, offset: Int): String =
    spec match
      case ColorSpec.Palette(i) => ansiDimPalette(i)
      case ColorSpec.Rainbow    => rainbowDimPalette(offset % rainbowDimPalette.size)

  /** Rainbow names shift hue per message so they shimmer down the scrollback. */
  private def shimmer(time: String): Int =
    math.floorMod(time.hashCode, rainbowPalette.size)

object LiveUi:
  def apply[F[_]: Async: Console](
      mutex: Mutex[F],
      state: Ref[F, ClientState[F]],
      ictl: InputCtl[F],
      blockLines: Ref[F, Int],
      pty: Boolean,
      termSize: Ref[F, Option[(Int, Int)]],
      scrollback: Ref[F, Vector[String]],
      ansi: Ansi,
      highlighter: Highlighter,
      terminal: Terminal[F]
  ): Ui[F] =
    new LiveUi[F](
      mutex,
      state,
      ictl,
      blockLines,
      pty,
      termSize,
      scrollback,
      ansi,
      highlighter,
      terminal
    )
