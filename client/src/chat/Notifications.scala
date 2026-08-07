package chat

import cats.Applicative
import cats.effect.*
import cats.effect.syntax.all.*
import cats.mtl.Handle.allow
import cats.syntax.all.*
import org.typelevel.log4cats.LoggerFactory

import scala.concurrent.duration.*
import scala.sys.process.*

import Ansi.*

/** Desktop notifications: mentions, rate-limited `!ping` bursts and reminders. */
trait Notifications[F[_]]:
  def mentions(line: String, myUsername: String): F[Unit]
  def nudge(msg: String, state: Ref[F, ClientState[F]]): F[Unit]
  def send(title: String, body: String, urgency: String, timeout: Int = 5000): F[Unit]
  def reminder(msg: String, myUsername: String, ui: Ui[F]): F[Unit]

final class LiveNotifications[F[_]: Async: LoggerFactory] private (
    audio: Audio[F]
) extends Notifications[F]:
  private val logger = LoggerFactory[F].getLogger

  private val maxPingsPerWindow = 3
  private val pingWindow = 5.minutes

  override def mentions(line: String, myUsername: String): F[Unit] =
    val messagePattern = """^\[(\d{2}:\d{2}:\d{2})\] [✓?] ([^:]+): (.+)$""".r
    val mentionPattern = """@([\p{L}\p{M}\p{N}_-]+)""".r

    line match
      case messagePattern(_, sender, content) =>
        val mentions = mentionPattern.findAllMatchIn(content).map(_.group(1)).toSet
        val direct = mentions.exists(_.equalsIgnoreCase(myUsername))
        val everyone =
          mentions.exists(_.equalsIgnoreCase("all")) &&
            !sender.trim.equalsIgnoreCase(myUsername)
        if direct || everyone then
          send(
            title =
              if direct then s"Chat: $sender mentioned you"
              else s"Chat: $sender mentioned everyone",
            body = content,
            urgency = "normal"
          )
        else Applicative[F].unit
      case _ =>
        Applicative[F].unit

  override def nudge(msg: String, state: Ref[F, ClientState[F]]): F[Unit] =
    msg.split(":", 3) match
      case Array(_, sender, countStr) =>
        val requested = countStr.trim.toIntOption.getOrElse(1).max(1)
        Clock[F].monotonic.flatMap { now =>
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
              if allowed <= 0 then Applicative[F].unit else sendMultiplePings(sender, allowed)
            }
        }
      case _ => Applicative[F].unit

  override def send(
      title: String,
      body: String,
      urgency: String,
      timeout: Int = 5000
  ): F[Unit] = {
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

    Sync[F].blocking(command.!).void.handleErrorWith { e =>
      logger.error(s"[Notification Error] ${e.getMessage}") *>
        logger.info(s"[Notification] $title: $body")
    } <* playNotificationSound(urgency == "critical").start.void
  }

  override def reminder(msg: String, myUsername: String, ui: Ui[F]): F[Unit] =
    msg.split(":", 5) match
      case Array(_, from, hh, mm, text) =>
        val fromSelf = from.equalsIgnoreCase(myUsername)
        val title = if fromSelf then "⏰ Reminder" else s"⏰ Reminder from $from"
        val line =
          if fromSelf then s"$serverColor⏰ Reminder (set for $hh:$mm): $text$ansiReset"
          else s"$serverColor⏰ Reminder from $from (set for $hh:$mm): $text$ansiReset"
        ui.printLine(line) *>
          send(title, text, urgency = "critical", timeout = 0)
      case _ => Applicative[F].unit

  private def sendMultiplePings(sender: String, count: Int): F[Unit] =
    val delay = 500.milliseconds

    (1 to count).toList.traverse_ { i =>
      send(
        title = s"🔔 Ping from $sender (${i}/$count)",
        body = s"$sender pinged you",
        urgency = "critical",
        timeout = 0
      ) >> Temporal[F].sleep(delay)
    }

  private def playNotificationSound(critical: Boolean): F[Unit] =
    allow[AudioError] {
      audio.playTone(critical)
    }.rescue(err => logger.warn(s"[Notification Sound] ${err.message}"))

object LiveNotifications:
  def apply[F[_]: Async: LoggerFactory](audio: Audio[F]): Notifications[F] =
    new LiveNotifications[F](audio)
