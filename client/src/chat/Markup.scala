package chat

/** Wire-format of a rendered chat line and the fenced/inline code snippets it can carry. */
object Markup:
  val displayPattern =
    """^\[(\d{2}:\d{2}:\d{2})\] ([✓?]) ([^:]+): (.*)$""".r

  val fence = "'''"

  private val fences = List("'''", "```")

  private val fenceOpenPattern = """^(?:'''|```)([A-Za-z0-9_+-]*)\s*$""".r

  val codeHeaderPattern = """^\[code — (\d+) lines\]$""".r

  def fenceLang(line: String): Option[String] =
    line match
      case fenceOpenPattern(lang) => Some(lang)
      case _                      => None

  private def isFenceClose(line: String): Boolean =
    fences.contains(line.trim)

  def pasteCodeLang(text: String): Option[String] =
    text.split("\n", -1).toList match
      case first :: rest if rest.nonEmpty && isFenceClose(rest.last) => fenceLang(first)
      case _                                                         => None

  def inlineCode(content: String): Option[String] =
    fences.collectFirst {
      case f if content.length > 2 * f.length && content.startsWith(f) && content.endsWith(f) =>
        content.substring(f.length, content.length - f.length)
    }

final case class PendingPaste(text: String, codeLang: Option[String])

final case class CodeAccum(remaining: Int, body: Vector[String])
