package chat

import cats.effect.*
import cats.syntax.all.*
import fs2.io.file.Files as Fs2Files
import fs2.io.file.Path as Fs2Path

import java.nio.file.Paths

/** Tab completion for commands, mentions, emoji shortcodes and `/sendfile` paths. */
object Completion:
  private val clientCommands = List(
    "!remind",
    "!reminders",
    "/acceptfile",
    "/help",
    "/mute",
    "/nick",
    "/quit",
    "/rejectfile",
    "/sendfile",
    "/voice",
    "/voicetest"
  )

  private val adminCommands = List("/ban", "/kick", "/unmute")

  private def splitLastToken(s: String): (String, String) =
    val i = s.lastIndexOf(' ')
    (s.take(i + 1), s.drop(i + 1))

  private def commonPrefix(xs: List[String]): String =
    xs.reduce { (a, b) =>
      a.take(a.zip(b).takeWhile(_ == _).length)
    }

  private def completionFor(inp: String, st: ClientState): IO[List[String]] =
    val (head, token) = splitLastToken(inp)
    if head.isEmpty && (token.startsWith("/") || token.startsWith("!")) then
      val cmds =
        if st.isAdmin then (clientCommands ++ adminCommands).sorted else clientCommands
      IO.pure(cmds.filter(_.startsWith(token)))
    else if token.startsWith("@") then
      val p = token.drop(1).toLowerCase
      IO.pure(st.onlineUsers.filter(_.toLowerCase.startsWith(p)).sorted.map("@" + _))
    else if token.startsWith(":") && token.length > 1 then
      IO.pure(
        Emoji.shortcodes.keys.toList.filter(_.startsWith(token.drop(1))).sorted.map(k => s":$k:")
      )
    else if inp.startsWith("/sendfile ") && head.nonEmpty then completePath(token)
    else IO.pure(Nil)

  private def completePath(token: String): IO[List[String]] =
    IO {
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
    }.flatMap { (dir, prefix, keep) =>
      Fs2Files[IO]
        .list(Fs2Path.fromNioPath(dir))
        .evalMap(p => Fs2Files[IO].isDirectory(p).map(d => (p.fileName.toString, d)))
        .compile
        .toList
        .map(
          _.filter((name, _) => name.startsWith(prefix))
            .sortBy(_._1)
            .map((name, isDir) => keep + name + (if isDir then "/" else ""))
        )
    }.handleError(_ => Nil)

  def complete(state: Ref[IO, ClientState], ui: Ui, ictl: InputCtl): IO[Unit] =
    ictl.pendingPaste.get.flatMap {
      case Some(_) => IO.unit
      case None =>
        (ictl.text.get, state.get).flatMapN { (inp, st) =>
          completionFor(inp, st).flatMap {
            case Nil => ictl.hint.set(None) *> ui.refreshInput
            case single :: Nil =>
              val done = if single.endsWith("/") then single else single + " "
              ictl.text.set(splitLastToken(inp)._1 + done) *>
                ictl.hint.set(None) *>
                ui.refreshInput
            case candidates =>
              val (head, token) = splitLastToken(inp)
              val lcp = commonPrefix(candidates)
              val extended = if lcp.length > token.length then head + lcp else inp
              ictl.text.set(extended) *>
                ictl.hint.set(Some(candidates.mkString("  "))) *>
                ui.refreshInput
          }
        }
    }
