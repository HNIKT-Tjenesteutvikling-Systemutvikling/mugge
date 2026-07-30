package chat

import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.*

import scala.concurrent.duration.*

/** Keyboard side of a session: turns tokens into prompt edits and, on submit, into wire lines. */
object UserInput:
  private val maxPasteLines = 100
  private val maxPasteChars = 8 * 1024
  private val pasteLineDelay = 400.milliseconds

  def readFromUser(
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      ictl: InputCtl,
      voiceRef: Ref[IO, Option[Voice]]
  ): Stream[IO, Nothing] =
    if !ui.isTty then
      Stream
        .repeatEval(Console[IO].readLine.map(Option(_)))
        .evalMap {
          case None    => halt.complete(Right(())).void
          case Some(s) => dispatchLine(s.trim, outgoingQueue, halt, state, ui, voiceRef)
        }
        .drain
    else
      InputToken
        .tokenize(
          fs2.io
            .stdin[IO](64)
            .through(text.utf8.decode)
            .flatMap(chunk => Stream.emits(chunk.toList))
        )
        .evalMap(tok => handleToken(tok, outgoingQueue, halt, state, ui, ictl, voiceRef))
        .drain

  private def handleToken(
      tok: InputToken,
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      ictl: InputCtl,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    tok match
      case InputToken.PasteStart => ictl.paste.set(Some((Nil, 0)))
      case InputToken.PasteEnd   => finishPaste(outgoingQueue, ui, ictl)
      case InputToken.HistoryPrev =>
        ictl.paste.get.flatMap {
          case Some(_) => IO.unit
          case None    => recallHistory(back = true, ui, ictl)
        }
      case InputToken.HistoryNext =>
        ictl.paste.get.flatMap {
          case Some(_) => IO.unit
          case None    => recallHistory(back = false, ui, ictl)
        }
      case InputToken.Newline =>
        ictl.paste.get.flatMap {
          case Some(_) => IO.unit
          case None    => insertInputNewline(outgoingQueue, ui, ictl)
        }
      case InputToken.Ch(c) =>
        ictl.paste.get.flatMap {
          case Some(_) =>
            val ch = if c == '\r' then '\n' else c
            if ch == '\n' || ch == '\t' || ch >= ' ' then
              ictl.paste.update(_.map { (cs, n) =>
                if n < maxPasteChars then (ch :: cs, n + 1) else (cs, n + 1)
              })
            else IO.unit
          case None => handleInputChar(c, outgoingQueue, halt, state, ui, ictl, voiceRef)
        }

  private def recallHistory(back: Boolean, ui: Ui, ictl: InputCtl): IO[Unit] =
    ictl.pendingPaste.get.flatMap {
      case Some(_) => IO.unit
      case None =>
        ictl.text.get
          .flatMap(cur =>
            ictl.history.modify(h =>
              if back then InputHistory.prev(h, cur) else InputHistory.next(h)
            )
          )
          .flatMap {
            case None => IO.unit
            case Some(line) =>
              ictl.hint.set(None) *> ictl.text.set(line) *> ui.refreshInput
          }
    }

  private def insertInputNewline(
      outgoingQueue: Queue[IO, String],
      ui: Ui,
      ictl: InputCtl
  ): IO[Unit] =
    ictl.hint.set(None) *>
      ictl.text.update(_ + "\n") *>
      ui.refreshInput *>
      startTyping(outgoingQueue, ictl.composing)

  private def finishPaste(
      outgoingQueue: Queue[IO, String],
      ui: Ui,
      ictl: InputCtl
  ): IO[Unit] =
    ictl.paste.getAndSet(None).flatMap {
      case None => IO.unit
      case Some((cs, n)) =>
        val text = cs.reverse.mkString.stripSuffix("\n")
        val lines = text.count(_ == '\n') + 1
        if n > maxPasteChars || lines > maxPasteLines then
          ui.printLine(
            s"Paste dropped: too large (max $maxPasteChars chars / $maxPasteLines lines)."
          )
        else if !text.contains('\n') then
          ictl.text.update(_ + text) *>
            ui.refreshInput *>
            startTyping(outgoingQueue, ictl.composing)
        else
          ictl.pendingPaste.set(Some(PendingPaste(text, Markup.pasteCodeLang(text)))) *>
            ui.refreshInput *>
            startTyping(outgoingQueue, ictl.composing)
    }

  private def sendPasteBlock(paste: PendingPaste, outgoingQueue: Queue[IO, String]): IO[Unit] =
    val lines = paste.text.split("\n", -1).toList
    val header =
      if paste.codeLang.isDefined then s"[code — ${lines.size} lines]"
      else s"[paste — ${lines.size} lines]"
    val block = header :: lines.map("│ " + _)
    block.traverse_(l => outgoingQueue.offer(l) *> IO.sleep(pasteLineDelay)).start.void

  private def submitInput(
      line: String,
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    if line.contains('\n') then
      val body = line.stripSuffix("\n")
      if body.trim.isEmpty then IO.unit
      else sendPasteBlock(PendingPaste(body, Markup.pasteCodeLang(body)), outgoingQueue)
    else dispatchLine(line.trim, outgoingQueue, halt, state, ui, voiceRef)

  private def handleInputChar(
      ch: Char,
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      ictl: InputCtl,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    ch match
      case '\n' | '\r' =>
        (ictl.text.getAndSet(""), ictl.pendingPaste.getAndSet(None), ictl.hint.getAndSet(None))
          .flatMapN { (line, paste, _) =>
            ictl.history.update(InputHistory.record(_, line.stripSuffix("\n"))) *>
              ui.refreshInput *>
              stopTyping(outgoingQueue, ictl.composing) *>
              submitInput(line, outgoingQueue, halt, state, ui, voiceRef) *>
              paste.traverse_(sendPasteBlock(_, outgoingQueue))
          }
      case '\t' =>
        Completion.complete(state, ui, ictl)
      case '\u007f' | '\b' =>
        ictl.hint.set(None) *>
          ictl.pendingPaste.getAndSet(None).flatMap {
            case Some(_) => ui.refreshInput // Backspace discards a pending paste first
            case None =>
              ictl.text
                .updateAndGet { s =>
                  if s.isEmpty then s
                  else s.dropRight(if Character.isLowSurrogate(s.last) then 2 else 1)
                }
                .flatMap { s =>
                  ui.refreshInput *>
                    (if s.isEmpty then stopTyping(outgoingQueue, ictl.composing) else IO.unit)
                }
          }
      case '\u0010' => recallHistory(back = true, ui, ictl)
      case '\u000e' => recallHistory(back = false, ui, ictl)
      case '\u0004' => // Ctrl-D on an empty prompt behaves like /quit
        if Config.serviceMode then ui.printLine(Config.quitHint) else halt.complete(Right(())).void
      case '\u000c' => // Ctrl-L (and dtach's ctrl_l attach hook) repaints
        ui.redraw
      case c if c >= ' ' =>
        ictl.hint.set(None) *>
          ictl.text.update(_ + c) *>
          ui.refreshInput *>
          startTyping(outgoingQueue, ictl.composing)
      case _ => IO.unit

  private def dispatchLine(
      line: String,
      outgoingQueue: Queue[IO, String],
      halt: Deferred[IO, Either[Throwable, Unit]],
      state: Ref[IO, ClientState],
      ui: Ui,
      voiceRef: Ref[IO, Option[Voice]]
  ): IO[Unit] =
    if line.isEmpty then IO.unit
    else if line == "/quit" then
      if Config.serviceMode then ui.printLine(Config.quitHint) else halt.complete(Right(())).void
    else if line.startsWith("/sendfile ") then
      FileTransfer.prepareSend(line.drop(10), state, outgoingQueue, ui)
    else if line == "/voice" then Voice.toggle(state, outgoingQueue, ui, voiceRef)
    else if line == "/voicetest" then Voice.toggleTest(state, outgoingQueue, ui, voiceRef)
    else if line == "/mute" then Voice.toggleMute(state, ui, voiceRef)
    else if line.equalsIgnoreCase("yes") || line.equalsIgnoreCase("no") then
      Assist.answerConsent(line.equalsIgnoreCase("yes"), line, state, outgoingQueue, ui)
    else if line.startsWith("/") then outgoingQueue.offer(line)
    else if Markup.inlineCode(line).isDefined then outgoingQueue.offer(line)
    else outgoingQueue.offer(Emoji.expand(line))

  private def startTyping(
      outgoingQueue: Queue[IO, String],
      composing: Ref[IO, Boolean]
  ): IO[Unit] =
    composing.getAndSet(true).flatMap(was => if was then IO.unit else outgoingQueue.offer("TYPING"))

  private def stopTyping(
      outgoingQueue: Queue[IO, String],
      composing: Ref[IO, Boolean]
  ): IO[Unit] =
    composing
      .getAndSet(false)
      .flatMap(was => if was then outgoingQueue.offer("TYPINGSTOP") else IO.unit)
