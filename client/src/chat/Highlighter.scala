package chat

trait Highlighter:
  def normalizeLang(lang: String): String
  def highlight(lang: String, line: String): String

object Highlighter:
  private[chat] val reset = "\u001b[0m"
  private[chat] val keywordColor = "\u001b[38;5;175m"
  private[chat] val stringColor = "\u001b[38;5;150m"
  private[chat] val numberColor = "\u001b[38;5;180m"
  private[chat] val commentColor = "\u001b[38;5;244m"

  private[chat] val scalaKeywords = Set(
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

  private[chat] val nixKeywords =
    Set("assert", "else", "if", "in", "inherit", "let", "or", "rec", "then", "with")

  private[chat] val pythonKeywords = Set(
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

  private[chat] val elispKeywords = Set(
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

  private[chat] val javaKeywords = Set(
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

  private[chat] val javascriptKeywords = Set(
    "async",
    "await",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "from",
    "function",
    "get",
    "if",
    "import",
    "in",
    "instanceof",
    "let",
    "new",
    "null",
    "of",
    "return",
    "set",
    "static",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "undefined",
    "var",
    "void",
    "while",
    "with",
    "yield"
  )

  private[chat] val typescriptKeywords = javascriptKeywords ++ Set(
    "abstract",
    "any",
    "as",
    "asserts",
    "bigint",
    "boolean",
    "declare",
    "enum",
    "implements",
    "infer",
    "interface",
    "is",
    "keyof",
    "namespace",
    "never",
    "number",
    "object",
    "override",
    "private",
    "protected",
    "public",
    "readonly",
    "satisfies",
    "string",
    "symbol",
    "type",
    "unique",
    "unknown"
  )

  private[chat] val haskellKeywords = Set(
    "as",
    "case",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "else",
    "family",
    "forall",
    "foreign",
    "hiding",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "mdo",
    "module",
    "newtype",
    "of",
    "pattern",
    "qualified",
    "rec",
    "then",
    "type",
    "where"
  )

  private[chat] val rustKeywords = Set(
    "as",
    "async",
    "await",
    "break",
    "const",
    "continue",
    "crate",
    "dyn",
    "else",
    "enum",
    "extern",
    "false",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "match",
    "mod",
    "move",
    "mut",
    "pub",
    "ref",
    "return",
    "self",
    "Self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "type",
    "union",
    "unsafe",
    "use",
    "where",
    "while"
  )

  private[chat] val jsQuotes = Set('"', '\'', '`')

final class LiveHighlighter private () extends Highlighter:
  import Highlighter.*

  override def normalizeLang(lang: String): String =
    lang.trim.toLowerCase match
      case "sc" | "scala"                => "scala"
      case "nix"                         => "nix"
      case "el" | "elisp" | "emacs-lisp" => "elisp"
      case "py" | "python"               => "python"
      case "java"                        => "java"
      case "js" | "javascript"           => "javascript"
      case "ts" | "typescript"           => "typescript"
      case "hs" | "haskell"              => "haskell"
      case "rs" | "rust"                 => "rust"
      case other                         => other

  override def highlight(lang: String, line: String): String =
    normalizeLang(lang) match
      case "scala"      => lex(line, scalaKeywords, "//")
      case "nix"        => lex(line, nixKeywords, "#")
      case "python"     => lex(line, pythonKeywords, "#")
      case "elisp"      => lex(line, elispKeywords, ";")
      case "java"       => lex(line, javaKeywords, "//")
      case "javascript" => lex(line, javascriptKeywords, "//", jsQuotes)
      case "typescript" => lex(line, typescriptKeywords, "//", jsQuotes)
      case "haskell"    => lex(line, haskellKeywords, "--")
      case "rust"       => lex(line, rustKeywords, "//")
      case _            => line

  private def isWordStart(c: Char): Boolean = c.isLetter || c == '_'
  private def isWordPart(c: Char): Boolean = c.isLetterOrDigit || c == '_' || c == '-'

  private def lex(
      line: String,
      keywords: Set[String],
      commentPrefix: String,
      quotes: Set[Char] = Set('"')
  ): String =
    @annotation.tailrec
    def spanEnd(j: Int, pred: Char => Boolean): Int =
      if j < line.length && pred(line.charAt(j)) then spanEnd(j + 1, pred) else j

    @annotation.tailrec
    def stringEnd(j: Int, quote: Char): Int =
      if j >= line.length then j
      else if line.charAt(j) == '\\' && j + 1 < line.length then stringEnd(j + 2, quote)
      else if line.charAt(j) == quote then j + 1
      else stringEnd(j + 1, quote)

    def paint(color: String, text: String): String = color + text + reset

    @annotation.tailrec
    def loop(i: Int, acc: String): String =
      if i >= line.length then acc
      else if line.startsWith(commentPrefix, i) then acc + paint(commentColor, line.substring(i))
      else
        val c = line.charAt(i)
        if quotes.contains(c) then
          val stop = stringEnd(i + 1, c)
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

object LiveHighlighter:
  def apply(): Highlighter = new LiveHighlighter()
