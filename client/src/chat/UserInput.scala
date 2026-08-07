package chat

import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.*

import scala.concurrent.duration.*

/** Keyboard side of a session: turns tokens into prompt edits and, on submit, into wire lines. */
trait UserInput[F[_]]:
  def readFromUser(
      outgoingQueue: Queue[F, String],
      halt: Deferred[F, Either[Throwable, Unit]],
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      ictl: InputCtl[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): Stream[F, Nothing]

  /** Also the entry point for lines arriving on the IPC socket, so a frontend behaves exactly like
    * someone typing at the prompt.
    */
  def dispatchLine(
      line: String,
      outgoingQueue: Queue[F, String],
      halt: Deferred[F, Either[Throwable, Unit]],
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit]

final class LiveUserInput[F[_]: Async: Console] private (
    completion: Completion[F],
    fileTransfer: FileTransfer[F],
    voice: Voice[F],
    assist: Assist[F],
    markup: Markup,
    emoji: Emoji,
    tokenizer: Tokenizer[F]
) extends UserInput[F]:
  private val maxPasteLines = 100
  private val maxPasteChars = 8 * 1024
  private val pasteLineDelay = 400.milliseconds

  override def readFromUser(
      outgoingQueue: Queue[F, String],
      halt: Deferred[F, Either[Throwable, Unit]],
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      ictl: InputCtl[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): Stream[F, Nothing] =
    if !ui.isTty then
      Stream
        .repeatEval(Console[F].readLine.map(Option(_)))
        .evalMap {
          case None    => halt.complete(Right(())).void
          case Some(s) => dispatchLine(s.trim, outgoingQueue, halt, state, ui, voiceRef)
        }
        .drain
    else
      tokenizer
        .tokenize(
          fs2.io
            .stdin[F](64)
            .through(text.utf8.decode)
            .flatMap(chunk => Stream.emits(chunk.toList))
        )
        .evalMap(tok => handleToken(tok, outgoingQueue, halt, state, ui, ictl, voiceRef))
        .drain

  private def handleToken(
      tok: InputToken,
      outgoingQueue: Queue[F, String],
      halt: Deferred[F, Either[Throwable, Unit]],
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      ictl: InputCtl[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit] =
    tok match
      case InputToken.PasteStart => ictl.paste.set(Some((Nil, 0)))
      case InputToken.PasteEnd   => finishPaste(outgoingQueue, ui, ictl)
      case InputToken.HistoryPrev =>
        ictl.paste.get.flatMap {
          case Some(_) => ().pure[F]
          case None    => recallHistory(back = true, ui, ictl)
        }
      case InputToken.HistoryNext =>
        ictl.paste.get.flatMap {
          case Some(_) => ().pure[F]
          case None    => recallHistory(back = false, ui, ictl)
        }
      case InputToken.Newline =>
        ictl.paste.get.flatMap {
          case Some(_) => ().pure[F]
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
            else ().pure[F]
          case None => handleInputChar(c, outgoingQueue, halt, state, ui, ictl, voiceRef)
        }

  private def recallHistory(back: Boolean, ui: Ui[F], ictl: InputCtl[F]): F[Unit] =
    ictl.pendingPaste.get.flatMap {
      case Some(_) => ().pure[F]
      case None =>
        ictl.text.get
          .flatMap(cur =>
            ictl.history.modify(h =>
              if back then InputHistory.prev(h, cur) else InputHistory.next(h)
            )
          )
          .flatMap {
            case None => ().pure[F]
            case Some(line) =>
              ictl.hint.set(None) *> ictl.text.set(line) *> ui.refreshInput
          }
    }

  private def insertInputNewline(
      outgoingQueue: Queue[F, String],
      ui: Ui[F],
      ictl: InputCtl[F]
  ): F[Unit] =
    ictl.hint.set(None) *>
      ictl.text.update(_ + "\n") *>
      ui.refreshInput *>
      startTyping(outgoingQueue, ictl.composing)

  private def finishPaste(
      outgoingQueue: Queue[F, String],
      ui: Ui[F],
      ictl: InputCtl[F]
  ): F[Unit] =
    ictl.paste.getAndSet(None).flatMap {
      case None => ().pure[F]
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
          ictl.pendingPaste.set(Some(PendingPaste(text, markup.pasteCodeLang(text)))) *>
            ui.refreshInput *>
            startTyping(outgoingQueue, ictl.composing)
    }

  private def sendPasteBlock(paste: PendingPaste, outgoingQueue: Queue[F, String]): F[Unit] =
    val lines = paste.text.split("\n", -1).toList
    val header =
      if paste.codeLang.isDefined then s"[code — ${lines.size} lines]"
      else s"[paste — ${lines.size} lines]"
    val block = header :: lines.map("│ " + _)
    block.traverse_(l => outgoingQueue.offer(l) *> Temporal[F].sleep(pasteLineDelay)).start.void

  private def submitInput(
      line: String,
      outgoingQueue: Queue[F, String],
      halt: Deferred[F, Either[Throwable, Unit]],
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit] =
    if line.contains('\n') then
      val body = line.stripSuffix("\n")
      if body.trim.isEmpty then ().pure[F]
      else sendPasteBlock(PendingPaste(body, markup.pasteCodeLang(body)), outgoingQueue)
    else dispatchLine(line.trim, outgoingQueue, halt, state, ui, voiceRef)

  private def handleInputChar(
      ch: Char,
      outgoingQueue: Queue[F, String],
      halt: Deferred[F, Either[Throwable, Unit]],
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      ictl: InputCtl[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit] =
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
        completion.complete(state, ui, ictl)
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
                    (if s.isEmpty then stopTyping(outgoingQueue, ictl.composing) else ().pure[F])
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
      case _ => ().pure[F]

  override def dispatchLine(
      line: String,
      outgoingQueue: Queue[F, String],
      halt: Deferred[F, Either[Throwable, Unit]],
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      voiceRef: Ref[F, Option[VoiceSession[F]]]
  ): F[Unit] =
    if line.isEmpty then ().pure[F]
    else if line == "/quit" then
      if Config.serviceMode then ui.printLine(Config.quitHint) else halt.complete(Right(())).void
    else if line.startsWith("/sendfile ") then
      fileTransfer.prepareSend(line.drop(10), state, outgoingQueue, ui)
    else if line == "/voice" then voice.toggle(state, outgoingQueue, ui, voiceRef)
    else if line == "/voicetest" then voice.toggleTest(state, outgoingQueue, ui, voiceRef)
    else if line == "/mute" then voice.toggleMute(state, ui, voiceRef)
    else if line.equalsIgnoreCase("yes") || line.equalsIgnoreCase("no") then
      assist.answerConsent(line.equalsIgnoreCase("yes"), line, state, outgoingQueue, ui)
    else if line.startsWith("/") then outgoingQueue.offer(line)
    else if markup.inlineCode(line).isDefined then outgoingQueue.offer(line)
    else outgoingQueue.offer(emoji.expand(line))

  private def startTyping(
      outgoingQueue: Queue[F, String],
      composing: Ref[F, Boolean]
  ): F[Unit] =
    composing
      .getAndSet(true)
      .flatMap(was => if was then ().pure[F] else outgoingQueue.offer("TYPING"))

  private def stopTyping(
      outgoingQueue: Queue[F, String],
      composing: Ref[F, Boolean]
  ): F[Unit] =
    composing
      .getAndSet(false)
      .flatMap(was => if was then outgoingQueue.offer("TYPINGSTOP") else ().pure[F])

object LiveUserInput:
  def apply[F[_]: Async: Console](
      completion: Completion[F],
      fileTransfer: FileTransfer[F],
      voice: Voice[F],
      assist: Assist[F],
      markup: Markup,
      emoji: Emoji,
      tokenizer: Tokenizer[F]
  ): UserInput[F] =
    new LiveUserInput[F](completion, fileTransfer, voice, assist, markup, emoji, tokenizer)
