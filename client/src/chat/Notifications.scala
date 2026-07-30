package chat

import cats.effect.*
import cats.mtl.Handle.allow
import cats.syntax.all.*
import org.typelevel.log4cats.Logger as TLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*
import scala.sys.process.*

import Ansi.*

/** Desktop notifications: mentions, rate-limited `!ping` bursts and reminders. */
object Notifications:
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  private val maxPingsPerWindow = 3
  private val pingWindow = 5.minutes

  def mentions(line: String, myUsername: String): IO[Unit] =
    val messagePattern = """^\[(\d{2}:\d{2}:\d{2})\] [✓?] ([^:]+): (.+)$""".r
    val mentionPattern = s"@(\\w+)".r

    line match
      case messagePattern(time, sender, content) =>
        if !content.startsWith("!ping") then
          val mentions = mentionPattern.findAllMatchIn(content).map(_.group(1)).toSet

          if mentions.exists(_.equalsIgnoreCase(myUsername)) then
            send(
              title = s"Chat: $sender mentioned you",
              body = content,
              urgency = "normal"
            )
          else IO.unit
        else IO.unit
      case _ =>
        IO.unit

  def pings(
      line: String,
      myUsername: String,
      state: Ref[IO, ClientState]
  ): IO[Unit] =
    val messagePattern = """^\[(\d{2}:\d{2}:\d{2})\] [✓?] ([^:]+): (.+)$""".r
    val pingPattern = """^!ping\s+@(\w+)(?:\s+(\d+))?""".r

    line match
      case messagePattern(time, sender, content) =>
        content match
          case pingPattern(targetUser, countStr) if targetUser.equalsIgnoreCase(myUsername) =>
            val requested = Option(countStr).flatMap(_.toIntOption).getOrElse(1).max(1)
            IO.monotonic.flatMap { now =>
              state
                .modify { st =>
                  val recent = st.pingHistory
                    .getOrElse(sender, Nil)
                    .filter(t => now - t < pingWindow)
                  val allowed = (maxPingsPerWindow - recent.size).max(0).min(requested)
                  val updated = recent ++ List.fill(allowed)(now)
                  (st.copy(pingHistory = st.pingHistory.updated(sender, updated)), allowed)
                }
                .flatMap { allowed =>
                  if allowed <= 0 then IO.unit
                  else sendMultiplePings(sender, time, allowed)
                }
            }
          case _ => IO.unit
      case _ =>
        IO.unit

  private def sendMultiplePings(sender: String, time: String, count: Int): IO[Unit] =
    val delay = 500.milliseconds

    (1 to count).toList.traverse_ { i =>
      send(
        title = s"🔔 Mention from $sender (${i}/$count)",
        body = s"$sender mentioned you at $time",
        urgency = "critical",
        timeout = 0
      ) >> IO.sleep(delay)
    }

  def send(
      title: String,
      body: String,
      urgency: String,
      timeout: Int = 5000
  ): IO[Unit] = {
    val command = Seq(
      "notify-send",
      "-u",
      urgency,
      "-i",
      "dialog-information",
      "-a",
      "Terminal Chat",
      "-t",
      timeout.toString,
      title,
      body
    )

    IO.blocking(command.!).void.handleErrorWith { e =>
      logger.error(s"[Notification Error] ${e.getMessage}") *>
        logger.info(s"[Notification] $title: $body")
    } <* playNotificationSound(urgency == "critical").start.void
  }

  private def playNotificationSound(critical: Boolean): IO[Unit] =
    allow[Audio.AudioError] {
      Audio.playTone(critical)
    }.rescue(err => logger.warn(s"[Notification Sound] ${err.message}"))

  def reminder(msg: String, myUsername: String, ui: Ui): IO[Unit] =
    // REMIND:<from>:<HH:MM>:<text> — text is last and may contain ':'.
    msg.split(":", 4) match
      case Array(_, from, hhmm, text) =>
        val fromSelf = from.equalsIgnoreCase(myUsername)
        val title = if fromSelf then "⏰ Reminder" else s"⏰ Reminder from $from"
        val line =
          if fromSelf then s"$serverColor⏰ Reminder (set for $hhmm): $text$ansiReset"
          else s"$serverColor⏰ Reminder from $from (set for $hhmm): $text$ansiReset"
        ui.printLine(line) *>
          send(title, text, urgency = "critical", timeout = 0)
      case _ => IO.unit
