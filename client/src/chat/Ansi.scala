package chat

import scala.util.matching.Regex

trait Ansi:
  def wrapAnsi(s: String, width: Int): List[String]
  def visibleWidth(s: String): Int
  def padVisible(s: String, width: Int): String
  def linkify(s: String): String

object Ansi:
  val ansiReset = "\u001b[0m"

  // Handed out in order, so the first dozen users get the twelve saturated
  // hues 30 degrees apart; the rest are pastels of the same wheel, which stay
  // apart by lightness. All are 6x6x6-cube codes so `dimmed` can derive the
  // message-body colour.
  private val paletteCodes: Vector[Int] = Vector(
    39, // blue
    208, // orange
    46, // green
    201, // magenta
    226, // yellow
    51, // cyan
    196, // red
    93, // violet
    118, // chartreuse
    49, // spring green
    198, // rose
    63, // indigo
    117, // sky
    222, // gold
    121, // mint
    219, // orchid
    210, // salmon
    87, // pale cyan
    192, // pale lime
    147, // periwinkle
    216, // peach
    158, // pale aqua
    218, // pink
    214 // amber
  )

  // Cube channel levels dimmed to roughly a third: 0,1,2,3,4,5 -> 0,1,1,1,2,2.
  private val dimLevel = Vector(0, 1, 1, 1, 2, 2)

  private def dimmed(code: Int): Int =
    val i = code - 16
    16 + 36 * dimLevel(i / 36) + 6 * dimLevel(i % 36 / 6) + dimLevel(i % 6)

  val ansiPalette: Vector[String] = paletteCodes.map(c => s"\u001b[38;5;${c}m")

  val ansiDimPalette: Vector[String] = paletteCodes.map(c => s"\u001b[38;5;${dimmed(c)}m")

  val serverColor = "\u001b[38;5;245m"

  private[chat] val osc8Prefix = "\u001b]8;;"
  private[chat] val osc8Close = s"$osc8Prefix\u001b\\"

  private[chat] val urlPattern: Regex = """https?://[^\s"'<>)\]]+""".r

final class LiveAnsi private () extends Ansi:
  import Ansi.osc8Close
  import Ansi.osc8Prefix
  import Ansi.urlPattern

  private def ansiSeqLength(s: String, start: Int): Int =
    if start + 1 >= s.length then 1
    else if s.charAt(start + 1) == '[' then
      val finalByte = s.indexWhere(c => c >= '@' && c <= '~', start + 2)
      if finalByte < 0 then s.length - start else finalByte - start + 1
    else if s.charAt(start + 1) == ']' then oscSeqLength(s, start)
    else 2

  // OSC runs to a BEL or an ST (ESC \), unlike the CSI final-byte rule above.
  private def oscSeqLength(s: String, start: Int): Int =
    @annotation.tailrec
    def loop(i: Int): Int =
      if i >= s.length then s.length - start
      else if s.charAt(i) == '\u0007' then i - start + 1
      else if s.charAt(i) == '\u001b' && i + 1 < s.length && s.charAt(i + 1) == '\\' then
        i - start + 2
      else loop(i + 1)
    loop(start + 2)

  private def isOsc8Open(s: String, start: Int, end: Int): Boolean =
    s.startsWith(osc8Prefix, start) && end - start > osc8Close.length

  // A hyperlink label runs up to its closing escape.
  private def linkCells(s: String, from: Int): Int =
    @annotation.tailrec
    def loop(i: Int, cells: Int): Int =
      if i >= s.length || s.charAt(i) == '\u001b' then cells
      else loop(i + Character.charCount(s.codePointAt(i)), cells + 1)
    loop(from, 0)

  override def wrapAnsi(s: String, width: Int): List[String] =
    @annotation.tailrec
    def loop(i: Int, cells: Int, row: String, acc: List[String]): List[String] =
      if i >= s.length then acc :+ row
      else if s.charAt(i) == '\u001b' then
        val next = i + ansiSeqLength(s, i)
        val link = if isOsc8Open(s, i, next) then linkCells(s, next) else 0
        // Push a whole URL to the next row rather than splitting it.
        if cells > 0 && link > 0 && link <= width && cells + link > width then
          loop(i, 0, "", acc :+ row)
        else loop(next, cells, row + s.substring(i, next), acc)
      else if cells == width then loop(i, 0, "", acc :+ row)
      else
        val n = Character.charCount(s.codePointAt(i))
        loop(i + n, cells + 1, row + s.substring(i, i + n), acc)
    loop(0, 0, "", Nil)

  override def visibleWidth(s: String): Int =
    @annotation.tailrec
    def loop(i: Int, w: Int): Int =
      if i >= s.length then w
      else if s.charAt(i) == '\u001b' then loop(i + ansiSeqLength(s, i), w)
      else loop(i + Character.charCount(s.codePointAt(i)), w + 1)
    loop(0, 0)

  override def padVisible(s: String, width: Int): String =
    val vw = visibleWidth(s)
    if vw >= width then s else s + " " * (width - vw)

  override def linkify(s: String): String =
    if s.indexOf("http") < 0 then s
    else
      urlPattern.replaceAllIn(
        s,
        m =>
          Regex.quoteReplacement(
            s"$osc8Prefix${m.matched}\u001b\\${m.matched}$osc8Close"
          )
      )

object LiveAnsi:
  def apply(): Ansi = new LiveAnsi()
