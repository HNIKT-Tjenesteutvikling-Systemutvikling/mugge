package chat

import cats.Applicative
import cats.syntax.all.*

/** Client-side handling of the server's REACT:/REACTIONS: control lines. */
trait Reactions[F[_]]:
  /** `payload` is "REACT:" already stripped: "<id>:<emoji>:<user>:added|removed". */
  def incoming(payload: String, ui: Ui[F], ipc: Ipc[F]): F[Unit]

  /** `payload` is "REACTIONS:" already stripped: "<id>:<emoji1>=<u1>,<u2>;<emoji2>=<u3>". */
  def snapshot(payload: String, ipc: Ipc[F]): F[Unit]

final class LiveReactions[F[_]: Applicative] private () extends Reactions[F]:
  override def incoming(payload: String, ui: Ui[F], ipc: Ipc[F]): F[Unit] =
    payload.split(":", 4) match
      case Array(idStr, emoji, user, addedOrRemoved) =>
        val added = addedOrRemoved == "added"
        idStr.toIntOption match
          case Some(id) =>
            val verb = if added then "reacted" else "removed their reaction"
            ui.printLine(s"  ↳ $user $verb $emoji to #$id") *> ipc.reaction(id, emoji, user, added)
          case None => Applicative[F].unit
      case _ => Applicative[F].unit

  override def snapshot(payload: String, ipc: Ipc[F]): F[Unit] =
    payload.split(":", 2) match
      case Array(idStr, rest) =>
        idStr.toIntOption match
          case Some(id) =>
            val counts = rest
              .split(";")
              .map(_.trim)
              .filter(_.nonEmpty)
              .flatMap(_.split("=", 2) match
                case Array(emoji, users) =>
                  Some(emoji -> users.split(",").map(_.trim).filter(_.nonEmpty).toSet)
                case _ => None
              )
              .toMap
            ipc.reactions(id, counts)
          case None => Applicative[F].unit
      case _ => Applicative[F].unit

object LiveReactions:
  def apply[F[_]: Applicative](): Reactions[F] = new LiveReactions[F]()
