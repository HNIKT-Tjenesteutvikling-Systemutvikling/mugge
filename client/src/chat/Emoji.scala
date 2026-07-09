package chat

import scala.util.matching.Regex

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

  private val pattern: Regex = """:([a-zA-Z0-9_+-]+):""".r

  private val emoticonPattern: Regex =
    emoticons.keys.toList
      .sortBy(-_.length)
      .map(Regex.quote)
      .mkString("""(?<=^|\s)(""", "|", """)(?=\s|$)""")
      .r

  def expand(line: String): String =
    val withShortcodes = pattern.replaceAllIn(
      line,
      m => Regex.quoteReplacement(shortcodes.getOrElse(m.group(1), m.matched))
    )
    emoticonPattern.replaceAllIn(
      withShortcodes,
      m => Regex.quoteReplacement(emoticons(m.group(1)))
    )
