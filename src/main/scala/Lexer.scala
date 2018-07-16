import java.io.{File, FileInputStream, PushbackInputStream}

import Error.{error, warning}
import Lexer.EOF
import Token._

class Lexer(var token: Token,
            var lineNum: Int,
            var ch: Char,
            val fin: PushbackInputStream,
            var count: Int,
            val fileName: String) {
  def parseIdentifier() = {
    import Lexer.{isDigit, isNodigit, tkWords}
    var temp = ch.toString
    getCh()
    while (isDigit(ch) || isNodigit(ch)) {
      temp += ch
      getCh()
    }
    token = Lexer.keyWords.map(t => (t._2, t._1)).getOrElse(temp, TK_IDENT)
    tkWords +:= (count, token, temp, lineNum)
  }

  def parseNum(): String = {
    var c: String = ""
    //    getCh()
    var flag = true
    do {
      c += ch
      getCh()
    } while (Lexer.isDigit(ch))
    if (ch == '.') {
      do {
        c += ch
        getCh()
      } while (Lexer.isDigit(ch))
    }
    c
  }

  def parseString(sep: Char): String = {
    var c: Char = ' '
    var str: String = sep.toString
    getCh()
    var flag = true
    while (flag) {
      if (ch == sep)
        flag = false
      else if (ch == '\\') {
        str += ch
        getCh()
        ch match {
          case '0' => c = '\0'
          case 'b' => c = '\b'
          case 't' => c = '\t'
          case 'n' => c = '\n'
          case 'f' => c = '\f'
          case 'r' => c = '\r'
          case '\'' => c = '\''
          case '\"' => c = '\"'
          case '\\' => c = '\\'
          case _ =>
            c = ch
            if (c >= '!' && c <= '~')
              warning("illegal escape characters", this)
            else
              warning(s"illegal escape characters$c", this)
        }
        str += ch
        getCh()
        str
      } else {
        str += ch
        getCh()
        str
      }
    }
    str += sep.toString
    getCh()
    str
  }

  def getToken()(implicit parser:Parser): Unit = {
    preprocess()
    ch match {
      case x if (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || x == '_' =>
        parseIdentifier()
      case x if x >= '0' && x <= '9' =>
        val num = parseNum()
        token = TK_CINT
        Lexer.tkWords +:= (count, token, num, lineNum)
      case '-' =>
        var pointsto = ch.toString
        getCh()
        if (ch == '>') {
          token = TK_POINTSTO
          pointsto += ch
          getCh()
        } else
          token = TK_MINUS
        Lexer.tkWords +:= (count, token, pointsto, lineNum)
      case '+' =>
        token = TK_PLUS
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '/' =>
        token = TK_DIVIDE
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '%' =>
        token = TK_MOD
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '=' =>
        var temp = ch.toString
        getCh()
        if (ch == '=') {
          token = TK_EQ
          temp += ch
          getCh()
        } else
          token = TK_ASSIGN
        Lexer.tkWords +:= (count, token, temp, lineNum)
      case '!' =>
        getCh()
        if (ch == '!') {
          token = TK_NEQ
          getCh()
        } else
          error("unsupported !", this)
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
      case '<' =>
        var le = ch.toString
        getCh()
        if (ch == '=') {
          token = TK_LEQ
          le += ch
          getCh()
        } else
          token = TK_LT
        Lexer.tkWords +:= (count, token, le, lineNum)
      case '>' =>
        var ge = ch.toString
        getCh()
        if (ch == '=') {
          token = TK_GEQ
          ge += ch
          getCh()
        } else
          token = TK_GT
        Lexer.tkWords +:= (count, token, ge, lineNum)
      case '.' =>
        var dot = ch.toString
        getCh()
        if (ch == '.') {
          dot += ch
          getCh()
          if (ch != '.')
            error("error", this)
          else {
            token = TK_ELLIPSIS
            dot += ch
          }
          getCh()
        } else
          token = TK_DOT
        Lexer.tkWords +:= (count, token, dot, lineNum)
      case '&' =>
        token = TK_AND
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case ';' =>
        token = TK_SEMICOLON
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case ']' =>
        token = TK_CLOSEBR
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '}' =>
        token = TK_END
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case ')' =>
        token = TK_CLOSEPA
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '{' =>
        token = TK_BEGIN
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '[' =>
        token = TK_OPENBR
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case ',' =>
        token = TK_COMMA
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '(' =>
        token = TK_OPENPA
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '*' =>
        token = TK_STAR
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '\'' =>
        val str = parseString(ch)
        token = TK_CCHAR
        Lexer.tkWords +:= (count, token, str, lineNum)
      case '\"' =>
        val str = parseString(ch)
        token = TK_CSTR
        Lexer.tkWords +:= (count, token, str, lineNum)
      case EOF =>
        token = TK_EOF
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
      case _ =>
        error("unknown characters", this)
        getCh()
    }
    parser.syntaxIndent()
  }

  def getCh() = {
    ch = fin.read().toChar
    count += 1
  }

  def ungetC(c: Char) = {
    count = count - 1
    fin.unread(1)
  }

  def preprocess() = {
    var flag = true
    while (flag) {
      if (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n')
        skipWhiteSpace()
      else if (ch == '/') {
        getCh()
        if (ch == '*') {
          parseComment()
        } else {
          ungetC(ch)
          ch = '/'
          flag = false
        }
      } else
        flag = false
    }
  }


  def parseComment() = {
    getCh()
    var flag1 = true
    var flag2 = true

    do {
      do {
        if (ch == '\n' || ch == '*' || ch == EOF)
          flag1 = false
        else
          getCh()
      } while (flag1)
      if (ch == '\n' || ch == '\r' || ch == '\t') {
        lineNum = lineNum + 1
        getCh()
      } else if (ch == '*') {
        getCh()
        if (ch == '/') {
          getCh()
          flag2 = false
        }
      } else {
        error("no end comment sign", this)
      }
    } while (flag2)
  }


  def skipWhiteSpace() = {
    var flag = false
    while (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n') {
      if (ch == '\r') {
        getCh()
        if (ch != '\n')
          flag = false
        lineNum += 1
      }
      //      println(ch)
      getCh()
    }
  }
}

object Lexer {
  val EOF: Int = -1
  val keyWords: Map[Value, String] = Map(
    TK_PLUS -> "+",
    TK_MINUS -> "-",
    TK_STAR -> "*",
    TK_DIVIDE -> "/",
    TK_MOD -> "%",
    TK_NEQ -> "!=",
    TK_EQ -> "==",
    TK_LT -> "<",
    TK_LEQ -> "<=",
    TK_GT -> ">",
    TK_GEQ -> ">=",
    TK_ASSIGN -> "=",
    TK_POINTSTO -> "->",
    TK_DOT -> ".",
    TK_AND -> "&",
    TK_OPENPA -> "(",
    TK_CLOSEPA -> ")",
    TK_OPENBR -> "[",
    TK_CLOSEBR -> "]",
    TK_BEGIN -> "{",
    TK_END -> "}",
    TK_SEMICOLON -> ";",
    TK_COMMA -> ",",
    TK_ELLIPSIS -> "...",
    TK_EOF -> "End_Of_File",
    TK_CINT -> "integer",
    TK_CCHAR -> "character",
    TK_CSTR -> "string",
    KW_CHAR -> "char",
    KW_SHORT -> "short",
    KW_INT -> "int",
    KW_VOID -> "void",
    KW_STRUCT -> "struct",
    KW_IF -> "if",
    KW_ELSE -> "else",
    KW_FOR -> "for",
    KW_CONTINUE -> "continue",
    KW_BREAK -> "break",
    KW_RETURN -> "return",
    KW_SIZEOF -> "sizeof",
    KW_ALIGN -> "__align",
    KW_CDECL -> "__cdecl",
    KW_STDCALL -> "__stdcall"
  )

  var tkWords: List[(Int, Value, String, Int ,Symbol)] = List()

  def readSourceFile(file: File): PushbackInputStream = {
    new PushbackInputStream(new FileInputStream(file))
  }


  def isNodigit(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  def isDigit(c: Char) = c >= '0' && c <= '9'

  def colorToken = {
    tkWords.reverse
      .groupBy(x => x._4)
      .toList.sortBy(y => y._1)
      .map(xs => toColor(xs._2)
        .reduce(_ + _) + "\r\n")
      .foreach(print)
  }

  def toColor(list: List[(Int, Value, String, Int)]) = {
    list.map(xs => color(xs))
  }

  def color(i: (Int, Value, String, Int)): String = {
    import io.AnsiColor._

    if (i._2 <= TK_AND) {
      s"${YELLOW}${BOLD}${i._3}${RESET}"
    } else if (i._2 <= TK_ELLIPSIS && i._2 > TK_AND) {
      s"${RED}${BOLD}${i._3}${RESET}"
    } else if (i._2 <= KW_CHAR && i._2 >= TK_CINT) {
      s"${GREEN}${BOLD}${i._3}${RESET}"
    } else {
      s"${BLUE}${BOLD}${i._3}${RESET}"
    }

  }


}
