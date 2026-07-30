package chat

import scala.util.matching.Regex

object Ansi:
  val ansiReset = "\u001b[0m"

  val ansiPalette: Vector[String] = Vector(
    "\u001b[38;5;39m",
    "\u001b[38;5;208m",
    "\u001b[38;5;46m",
    "\u001b[38;5;201m",
    "\u001b[38;5;226m",
    "\u001b[38;5;51m",
    "\u001b[38;5;196m",
    "\u001b[38;5;129m",
    "\u001b[38;5;118m",
    "\u001b[38;5;214m",
    "\u001b[38;5;45m",
    "\u001b[38;5;213m"
  )

  val ansiDimPalette: Vector[String] = Vector(
    "\u001b[38;5;25m",
    "\u001b[38;5;130m",
    "\u001b[38;5;28m",
    "\u001b[38;5;90m",
    "\u001b[38;5;100m",
    "\u001b[38;5;30m",
    "\u001b[38;5;88m",
    "\u001b[38;5;54m",
    "\u001b[38;5;64m",
    "\u001b[38;5;136m",
    "\u001b[38;5;24m",
    "\u001b[38;5;96m"
  )

  val serverColor = "\u001b[38;5;245m"

  private val osc8Prefix = "\u001b]8;;"
  private val osc8Close = s"$osc8Prefix\u001b\\"

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

  // Visible cells of a hyperlink label: the text runs up to its closing escape.
  private def linkCells(s: String, from: Int): Int =
    @annotation.tailrec
    def loop(i: Int, cells: Int): Int =
      if i >= s.length || s.charAt(i) == '\u001b' then cells
      else loop(i + Character.charCount(s.codePointAt(i)), cells + 1)
    loop(from, 0)

  def wrapAnsi(s: String, width: Int): List[String] =
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

  def visibleWidth(s: String): Int =
    @annotation.tailrec
    def loop(i: Int, w: Int): Int =
      if i >= s.length then w
      else if s.charAt(i) == '\u001b' then loop(i + ansiSeqLength(s, i), w)
      else loop(i + Character.charCount(s.codePointAt(i)), w + 1)
    loop(0, 0)

  def padVisible(s: String, width: Int): String =
    val vw = visibleWidth(s)
    if vw >= width then s else s + " " * (width - vw)

  private val urlPattern: Regex = """https?://[^\s"'<>)\]]+""".r

  def linkify(s: String): String =
    if s.indexOf("http") < 0 then s
    else
      urlPattern.replaceAllIn(
        s,
        m =>
          Regex.quoteReplacement(
            s"$osc8Prefix${m.matched}\u001b\\${m.matched}$osc8Close"
          )
      )
