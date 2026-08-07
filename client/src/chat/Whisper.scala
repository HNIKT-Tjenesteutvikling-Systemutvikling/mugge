package chat

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all.*

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import Ansi.*

/** Private messages, rendered distinctly from room chat and never persisted. */
trait Whisper[F[_]]:
  def incoming(msg: String, ui: Ui[F], ipc: Ipc[F]): F[Unit]
  def outgoing(msg: String, ui: Ui[F], ipc: Ipc[F]): F[Unit]

final class LiveWhisper[F[_]: Sync] private (
    ansi: Ansi,
    notifications: Notifications[F]
) extends Whisper[F]:
  private val whisperColor = "\u001b[38;5;177m"
  private val whisperDim = "\u001b[38;5;140m"
  private val timeFormat = DateTimeFormatter.ofPattern("HH:mm:ss")

  override def incoming(msg: String, ui: Ui[F], ipc: Ipc[F]): F[Unit] =
    msg.split(":", 3) match
      case Array(_, from, text) =>
        render(s"✉ $from → you", text, ui, ipc, incoming = true, peer = from) *>
          notifications.send(
            title = s"✉ Whisper from $from",
            body = text,
            urgency = "normal"
          )
      case _ => Applicative[F].unit

  override def outgoing(msg: String, ui: Ui[F], ipc: Ipc[F]): F[Unit] =
    msg.split(":", 3) match
      case Array(_, to, text) =>
        render(s"✉ you → $to", text, ui, ipc, incoming = false, peer = to)
      case _ => Applicative[F].unit

  private def render(
      label: String,
      text: String,
      ui: Ui[F],
      ipc: Ipc[F],
      incoming: Boolean,
      peer: String
  ): F[Unit] =
    Sync[F].delay(LocalTime.now().format(timeFormat)).flatMap { time =>
      ipc.whisper(time, incoming, peer, text) *>
        ui.printLine(
          s"[$time] $whisperColor$label$ansiReset: $whisperDim${ansi.linkify(text)}$ansiReset"
        )
    }

object LiveWhisper:
  def apply[F[_]: Sync](ansi: Ansi, notifications: Notifications[F]): Whisper[F] =
    new LiveWhisper[F](ansi, notifications)
