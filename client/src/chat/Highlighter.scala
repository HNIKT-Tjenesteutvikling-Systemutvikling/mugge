package chat

object Highlighter:
  private val reset = "\u001b[0m"
  private val keywordColor = "\u001b[38;5;175m"
  private val stringColor = "\u001b[38;5;150m"
  private val numberColor = "\u001b[38;5;180m"
  private val commentColor = "\u001b[38;5;244m"

  private val scalaKeywords = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "end",
    "enum",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "given",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "then",
    "trait",
    "true",
    "try",
    "type",
    "using",
    "val",
    "var",
    "while",
    "with",
    "yield",
    "null"
  )

  private val nixKeywords =
    Set("assert", "else", "if", "in", "inherit", "let", "or", "rec", "then", "with")

  private val pythonKeywords = Set(
    "and",
    "as",
    "assert",
    "async",
    "await",
    "break",
    "class",
    "continue",
    "def",
    "del",
    "elif",
    "else",
    "except",
    "False",
    "finally",
    "for",
    "from",
    "global",
    "if",
    "import",
    "in",
    "is",
    "lambda",
    "None",
    "nonlocal",
    "not",
    "or",
    "pass",
    "raise",
    "return",
    "True",
    "try",
    "while",
    "with",
    "yield"
  )

  private val elispKeywords = Set(
    "defun",
    "defvar",
    "defcustom",
    "defconst",
    "defmacro",
    "let",
    "let*",
    "lambda",
    "if",
    "when",
    "unless",
    "cond",
    "setq",
    "require",
    "provide",
    "progn",
    "interactive",
    "and",
    "or",
    "not",
    "while",
    "dolist",
    "dotimes"
  )

  private val javaKeywords = Set(
    "abstract",
    "assert",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extends",
    "final",
    "finally",
    "float",
    "for",
    "goto",
    "if",
    "implements",
    "import",
    "instanceof",
    "int",
    "interface",
    "long",
    "native",
    "new",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "static",
    "strictfp",
    "super",
    "switch",
    "synchronized",
    "this",
    "throw",
    "throws",
    "transient",
    "try",
    "void",
    "volatile",
    "while",
    "var",
    "record",
    "sealed",
    "permits",
    "yield",
    "true",
    "false",
    "null"
  )

  def normalizeLang(lang: String): String =
    lang.trim.toLowerCase match
      case "sc" | "scala"                => "scala"
      case "nix"                         => "nix"
      case "el" | "elisp" | "emacs-lisp" => "elisp"
      case "py" | "python"               => "python"
      case "java"                        => "java"
      case other                         => other

  def highlight(lang: String, line: String): String =
    normalizeLang(lang) match
      case "scala"  => lex(line, scalaKeywords, "//")
      case "nix"    => lex(line, nixKeywords, "#")
      case "python" => lex(line, pythonKeywords, "#")
      case "elisp"  => lex(line, elispKeywords, ";")
      case "java"   => lex(line, javaKeywords, "//")
      case _        => line

  private def isWordStart(c: Char): Boolean = c.isLetter || c == '_'
  private def isWordPart(c: Char): Boolean = c.isLetterOrDigit || c == '_' || c == '-'

  private def lex(line: String, keywords: Set[String], commentPrefix: String): String =
    @annotation.tailrec
    def spanEnd(j: Int, pred: Char => Boolean): Int =
      if j < line.length && pred(line.charAt(j)) then spanEnd(j + 1, pred) else j

    @annotation.tailrec
    def stringEnd(j: Int): Int =
      if j >= line.length then j
      else if line.charAt(j) == '\\' && j + 1 < line.length then stringEnd(j + 2)
      else if line.charAt(j) == '"' then j + 1
      else stringEnd(j + 1)

    def paint(color: String, text: String): String = color + text + reset

    @annotation.tailrec
    def loop(i: Int, acc: String): String =
      if i >= line.length then acc
      else if line.startsWith(commentPrefix, i) then acc + paint(commentColor, line.substring(i))
      else
        val c = line.charAt(i)
        if c == '"' then
          val stop = stringEnd(i + 1)
          loop(stop, acc + paint(stringColor, line.substring(i, stop)))
        else if c.isDigit then
          val stop = spanEnd(i, ch => ch.isLetterOrDigit || ch == '.')
          loop(stop, acc + paint(numberColor, line.substring(i, stop)))
        else if isWordStart(c) then
          val stop = spanEnd(i, isWordPart)
          val word = line.substring(i, stop)
          loop(stop, acc + (if keywords.contains(word) then paint(keywordColor, word) else word))
        else loop(i + 1, acc + c)

    loop(0, "")
