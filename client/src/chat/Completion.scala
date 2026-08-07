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
    "!remind",
    "!reminders",
    "/acceptfile",
    "/help",
    "/mute",
    "/nick",
    "/quit",
    "/r",
    "/rejectfile",
    "/sendfile",
    "/voice",
    "/voicetest",
    "/w"
  )

  private val adminCommands = List("/ban", "/kick", "/server", "/unmute")

  override def complete(state: Ref[F, ClientState[F]], ui: Ui[F], ictl: InputCtl[F]): F[Unit] =
    ictl.pendingPaste.get.flatMap {
      case Some(_) => Applicative[F].unit
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
