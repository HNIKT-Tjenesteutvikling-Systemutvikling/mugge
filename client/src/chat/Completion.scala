package chat

import cats.Applicative
import cats.effect.*
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path

import java.nio.file.Paths

/** Tab completion for commands, mentions, emoji shortcodes and `/sendfile` paths. */
trait Completion[F[_]]:
  def complete(state: Ref[F, ClientState[F]], ui: Ui[F], ictl: InputCtl[F]): F[Unit]

final class LiveCompletion[F[_]: Async: Files] private () extends Completion[F]:
  private val clientCommands = List(
    "!list",
    "!note",
    "!notes",
    "!ping",
    "!remind",
    "!reminders",
    "/acceptfile",
    "/admins",
    "/help",
    "/mute",
    "/nick",
    "/quit",
    "/r",
    "/rejectfile",
    "/sendfile",
    "/status",
    "/voice",
    "/voicetest",
    "/w"
  )

  private val adminCommands =
    List("/ban", "/caffeine", "/clearhistory", "/kick", "/server", "/unmute")

  /** Commands whose argument is a bare display name, not an `@mention`. */
  private val nameArgCommands = Set("/ban", "/kick")

  override def complete(state: Ref[F, ClientState[F]], ui: Ui[F], ictl: InputCtl[F]): F[Unit] =
    ictl.pendingPaste.get.flatMap {
      case Some(_) => Applicative[F].unit
      case None =>
        (ictl.text.get, ictl.cycle.get).flatMapN { (inp, cycle) =>
          cycle match
            case Some(c) if c.text == inp && c.candidates.nonEmpty => step(c, ui, ictl)
            case _                                                 => start(inp, state, ui, ictl)
        }
    }

  private def start(
      inp: String,
      state: Ref[F, ClientState[F]],
      ui: Ui[F],
      ictl: InputCtl[F]
  ): F[Unit] =
    state.get.flatMap(completionFor(inp, _)).flatMap {
      case Nil =>
        ictl.cycle.set(None) *> ictl.hint.set(None) *> ui.refreshInput
      case single :: Nil =>
        ictl.text.set(splitLastToken(inp)._1 + accept(single)) *>
          ictl.cycle.set(None) *>
          ictl.hint.set(None) *>
          ui.refreshInput
      case candidates =>
        val (head, token) = splitLastToken(inp)
        val fresh = CompletionCycle(head, candidates, -1, inp)
        val lcp = commonPrefix(candidates)
        if lcp.length > token.length then
          val extended = head + lcp
          ictl.text.set(extended) *>
            ictl.cycle.set(Some(fresh.copy(text = extended))) *>
            ictl.hint.set(Some(hintFor(candidates, -1))) *>
            ui.refreshInput
        else step(fresh, ui, ictl)
    }

  private def step(c: CompletionCycle, ui: Ui[F], ictl: InputCtl[F]): F[Unit] =
    val i = (c.index + 1) % c.candidates.size
    val text = c.head + accept(c.candidates(i))
    ictl.text.set(text) *>
      ictl.cycle.set(Some(c.copy(index = i, text = text))) *>
      ictl.hint.set(Some(hintFor(c.candidates, i))) *>
      ui.refreshInput

  private def accept(candidate: String): String =
    if candidate.endsWith("/") then candidate else candidate + " "

  private def hintFor(candidates: List[String], selected: Int): String =
    candidates.zipWithIndex
      .map((c, i) => if i == selected then s"[$c]" else c)
      .mkString("  ")

  private def splitLastToken(s: String): (String, String) =
    val i = s.lastIndexOf(' ')
    (s.take(i + 1), s.drop(i + 1))

  private def commonPrefix(xs: List[String]): String =
    xs.reduce { (a, b) =>
      a.take(a.zip(b).takeWhile(_ == _).length)
    }

  private def completionFor(inp: String, st: ClientState[F]): F[List[String]] =
    val (head, token) = splitLastToken(inp)
    if head.isEmpty && (token.startsWith("/") || token.startsWith("!")) then
      val cmds =
        if st.isAdmin then (clientCommands ++ adminCommands).sorted else clientCommands
      cmds.filter(_.startsWith(token)).pure[F]
    else if token.startsWith("@") then
      val p = token.drop(1).toLowerCase
      st.onlineUsers.filter(_.toLowerCase.startsWith(p)).sorted.map("@" + _).pure[F]
    else if token.startsWith(":") && token.length > 1 then
      Emoji.shortcodes.keys.toList
        .filter(_.startsWith(token.drop(1)))
        .sorted
        .map(k => s":$k:")
        .pure[F]
    else if inp.startsWith("/sendfile ") && head.nonEmpty then completePath(token)
    else if nameArgCommands.contains(head.trim) then
      st.onlineUsers.filter(_.toLowerCase.startsWith(token.toLowerCase)).sorted.pure[F]
    else List.empty[String].pure[F]

  private def completePath(token: String): F[List[String]] =
    Sync[F]
      .delay {
        val home = System.getProperty("user.home")
        val expanded =
          if token == "~" then home + "/"
          else if token.startsWith("~/") then home + token.drop(1)
          else token
        val slash = expanded.lastIndexOf('/')
        val (dir, prefix) =
          if slash < 0 then (Paths.get("."), expanded)
          else (Paths.get(expanded.take(slash + 1)), expanded.drop(slash + 1))
        val keep = token.take(token.lastIndexOf('/') + 1)
        (dir, prefix, keep)
      }
      .flatMap { (dir, prefix, keep) =>
        Files[F]
          .list(Path.fromNioPath(dir))
          .evalMap(p => Files[F].isDirectory(p).map(d => (p.fileName.toString, d)))
          .compile
          .toList
          .map(
            _.filter((name, _) => name.startsWith(prefix))
              .sortBy(_._1)
              .map((name, isDir) => keep + name + (if isDir then "/" else ""))
          )
      }
      .handleError(_ => Nil)

object LiveCompletion:
  def apply[F[_]: Async: Files](): Completion[F] = new LiveCompletion[F]()
