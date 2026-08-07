package chat

trait Markup:
  def fenceLang(line: String): Option[String]
  def pasteCodeLang(text: String): Option[String]
  def inlineCode(content: String): Option[String]

object Markup:
  val displayPattern =
    """^\[(\d{2}:\d{2}:\d{2})\] ([✓?]) ([^:]+): (.*)$""".r

  val fence = "'''"

  private[chat] val fences = List("'''", "```")

  private[chat] val fenceOpenPattern = """^(?:'''|```)([A-Za-z0-9_+-]*)\s*$""".r

  val codeHeaderPattern = """^\[code — (\d+) lines\]$""".r

final class LiveMarkup private () extends Markup:
  import Markup.fenceOpenPattern
  import Markup.fences

  override def fenceLang(line: String): Option[String] =
    line match
      case fenceOpenPattern(lang) => Some(lang)
      case _                      => None

  private def isFenceClose(line: String): Boolean =
    fences.contains(line.trim)

  override def pasteCodeLang(text: String): Option[String] =
    text.split("\n", -1).toList match
      case first :: rest if rest.nonEmpty && isFenceClose(rest.last) => fenceLang(first)
      case _                                                         => None

  override def inlineCode(content: String): Option[String] =
    fences.collectFirst {
      case f if content.length > 2 * f.length && content.startsWith(f) && content.endsWith(f) =>
        content.substring(f.length, content.length - f.length)
    }

object LiveMarkup:
  def apply(): Markup = new LiveMarkup()

final case class PendingPaste(text: String, codeLang: Option[String])

final case class CodeAccum(remaining: Int, body: Vector[String])
