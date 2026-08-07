package chat

import scala.util.matching.Regex

trait Emoji:
  def expand(line: String): String

object Emoji:
  val shortcodes: Map[String, String] = Map(
    "smile" -> "😄",
    "grin" -> "😁",
    "joy" -> "😂",
    "rofl" -> "🤣",
    "wink" -> "😉",
    "sweat_smile" -> "😅",
    "cry" -> "😢",
    "sob" -> "😭",
    "angry" -> "😠",
    "rage" -> "😡",
    "thinking" -> "🤔",
    "shrug" -> "🤷",
    "facepalm" -> "🤦",
    "eyes" -> "👀",
    "wave" -> "👋",
    "clap" -> "👏",
    "+1" -> "👍",
    "thumbsup" -> "👍",
    "-1" -> "👎",
    "thumbsdown" -> "👎",
    "ok" -> "👌",
    "pray" -> "🙏",
    "muscle" -> "💪",
    "heart" -> "❤️",
    "broken_heart" -> "💔",
    "fire" -> "🔥",
    "100" -> "💯",
    "tada" -> "🎉",
    "rocket" -> "🚀",
    "star" -> "⭐",
    "zap" -> "⚡",
    "boom" -> "💥",
    "skull" -> "💀",
    "ghost" -> "👻",
    "poop" -> "💩",
    "beer" -> "🍺",
    "beers" -> "🍻",
    "coffee" -> "☕",
    "pizza" -> "🍕",
    "bug" -> "🐛",
    "check" -> "✅",
    "x" -> "❌",
    "warning" -> "⚠️",
    "question" -> "❓",
    "music" -> "🎵",
    "mic" -> "🎤",
    "wrench" -> "🔧",
    "lock" -> "🔒",
    "bell" -> "🔔",
    "zzz" -> "💤"
  )

  // Classic emoticons, expanded only when they stand alone (surrounded by
  // whitespace or line edges) so URLs and times pass through untouched.
  val emoticons: Map[String, String] = Map(
    ":)" -> "🙂",
    ":(" -> "🙁",
    ":D" -> "😄",
    ":d" -> "😄",
    ":P" -> "😛",
    ":p" -> "😛",
    ";)" -> "😉",
    ":O" -> "😮",
    ":o" -> "😮",
    ":'(" -> "😢",
    ":/" -> "😕",
    ":*" -> "😘",
    "<3" -> "❤️",
    "xD" -> "😆",
    "XD" -> "😆",
    "8)" -> "😎"
  )

  private[chat] val pattern: Regex = """:([a-zA-Z0-9_+-]+):""".r

  private[chat] val emoticonPattern: Regex =
    emoticons.keys.toList
      .sortBy(-_.length)
      .map(Regex.quote)
      .mkString("""(?<=^|\s)(""", "|", """)(?=\s|$)""")
      .r

final class LiveEmoji private () extends Emoji:
  override def expand(line: String): String =
    val withShortcodes = Emoji.pattern.replaceAllIn(
      line,
      m => Regex.quoteReplacement(Emoji.shortcodes.getOrElse(m.group(1), m.matched))
    )
    Emoji.emoticonPattern.replaceAllIn(
      withShortcodes,
      m => Regex.quoteReplacement(Emoji.emoticons(m.group(1)))
    )

object LiveEmoji:
  def apply(): Emoji = new LiveEmoji()
