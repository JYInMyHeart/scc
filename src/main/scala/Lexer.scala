import java.io.{File, FileInputStream, PushbackInputStream}

import Error.{error, warning}
import Lexer.EOF
import Token.Token

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
    //    token = Token.TK_IDENT
    token = Lexer.keyWords.map(t => (t._2, t._1)).getOrElse(temp, Token.TK_IDENT)
    tkWords +:= (count, token, temp, lineNum)
  }

  def parseNum(): String = {
    var c: String = ""
    getCh()
    var flag = true
    while (flag) {
      if (Lexer.isDigit(ch)) {
        c += ch
        getCh()
      }
      else if (ch == ';') {
        flag = false
        ungetC(ch)
        c
      }
      else {
        error("undefined", fileName, this)
        flag = false
      }
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
          case _ => {
            c = ch
            if (c >= '!' && c <= '~')
              warning("illegal escape characters", fileName, this)
            else
              warning(s"illegal escape characters$c", fileName, this)
          }
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

  def getToken(): Unit = {
    preprocess()
    ch match {
      case x if (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || x == '_' =>
        parseIdentifier()
      case x if x >= '0' && x <= '9' =>
        parseNum()
        token = Token.TK_CINT
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
      case '-' =>
        getCh()
        if (ch == '>') {
          token = Token.TK_POINTSTO
          getCh()
        } else
          token = Token.TK_MINUS
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
      case '/' =>
        token = Token.TK_DIVIDE
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '%' =>
        token = Token.TK_MOD
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '=' =>
        var temp = ch.toString
        getCh()
        if (ch == '=') {
          token = Token.TK_EQ
          temp += ch
          getCh()
        } else
          token = Token.TK_ASSIGN
        Lexer.tkWords +:= (count, token, temp, lineNum)
      case '!' =>
        getCh()
        if (ch == '!') {
          token = Token.TK_NEQ
          getCh()
        } else
          error("unsupported !", fileName, this)
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
      case '<' =>
        getCh()
        if (ch == '=') {
          token = Token.TK_LEQ
          getCh()
        } else
          token = Token.TK_LT
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
      case '>' =>
        getCh()
        if (ch == '=') {
          token = Token.TK_GEQ
          getCh()
        } else
          token = Token.TK_GT
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
      case '.' =>
        getCh()
        if (ch == '.') {
          getCh()
          if (ch != '.')
            error("error", fileName, this)
          else
            token = Token.TK_ELLIPSIS
          getCh()
        } else
          token = Token.TK_DOT
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
      case '&' =>
        token = Token.TK_AND
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case ';' =>
        token = Token.TK_SEMICOLON
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case ']' =>
        token = Token.TK_CLOSEBR
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '}' =>
        token = Token.TK_END
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case ')' =>
        token = Token.TK_CLOSEPA
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '{' =>
        token = Token.TK_BEGIN
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '[' =>
        token = Token.TK_OPENBR
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case ',' =>
        token = Token.TK_COMMA
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '(' =>
        token = Token.TK_OPENPA
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '*' =>
        token = Token.TK_STAR
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
        getCh()
      case '\'' =>
        val str = parseString(ch)
        token = Token.TK_CCHAR
        Lexer.tkWords +:= (count, token, str, lineNum)
      //        tkValue = tkstr.data
      case '\"' =>
        val str = parseString(ch)
        token = Token.TK_CSTR
        Lexer.tkWords +:= (count, token, str, lineNum)
      case EOF =>
        token = Token.TK_EOF
        Lexer.tkWords +:= (count, token, ch.toString, lineNum)
      case _ =>
        error("unknown characters", fileName, this)
        getCh()
    }
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
        error("no end comment sign", fileName, this)
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
  val keyWords: Map[Token.Value, String] = Map(
    Token.TK_PLUS -> "+",
    Token.TK_MINUS -> "-",
    Token.TK_STAR -> "*",
    Token.TK_DIVIDE -> "/",
    Token.TK_MOD -> "%",
    Token.TK_NEQ -> "!=",
    Token.TK_EQ -> "==",
    Token.TK_LT -> "<",
    Token.TK_LEQ -> "<=",
    Token.TK_GT -> ">",
    Token.TK_GEQ -> ">=",
    Token.TK_ASSIGN -> "=",
    Token.TK_POINTSTO -> "->",
    Token.TK_DOT -> ".",
    Token.TK_AND -> "&",
    Token.TK_OPENPA -> "(",
    Token.TK_CLOSEPA -> ")",
    Token.TK_OPENBR -> "[",
    Token.TK_CLOSEBR -> "]",
    Token.TK_BEGIN -> "{",
    Token.TK_END -> "}",
    Token.TK_SEMICOLON -> ";",
    Token.TK_COMMA -> ",",
    Token.TK_ELLIPSIS -> "...",
    Token.TK_EOF -> "End_Of_File",
    Token.TK_CINT -> "integer",
    Token.TK_CCHAR -> "character",
    Token.TK_CSTR -> "string",
    Token.KW_CHAR -> "char",
    Token.KW_SHORT -> "short",
    Token.KW_INT -> "int",
    Token.KW_VOID -> "void",
    Token.KW_STRUCT -> "struct",
    Token.KW_IF -> "if",
    Token.KW_ELSE -> "else",
    Token.KW_FOR -> "for",
    Token.KW_CONTINUE -> "continue",
    Token.KW_BREAK -> "break",
    Token.KW_RETURN -> "return",
    Token.KW_SIZEOF -> "sizeof",
    Token.KW_ALIGN -> "__align",
    Token.KW_CDECL -> "__cdecl",
    Token.KW_STDCALL -> "__stdcall"
  )

  var tkWords: List[(Int, Token.Value, String, Int)] = List()

  def readSourceFile(file: File): PushbackInputStream = {
    new PushbackInputStream(new FileInputStream(file))
  }


  def isNodigit(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  def isDigit(c: Char) = c >= '0' && c <= '9'

  def colorToken = {
    def toColor = {
      import io.AnsiColor._
      tkWords.reverse.map(xs => {
        if (xs._2 <= Token.TK_AND) {
          s"${YELLOW}${BOLD}${xs._3}${RESET}"
        } else if (xs._2 <= Token.TK_ELLIPSIS && xs._2 > Token.TK_AND) {
          s"${RED}${BOLD}${xs._3}${RESET}"
        } else if (xs._2 <= Token.KW_CHAR && xs._2 >= Token.TK_CINT) {
          s"${GREEN}${BOLD}${xs._3}${RESET}"
        } else {
          s"${BLUE}${BOLD}${xs._3}${RESET} "
        }
      })
    }

    toColor.foreach(print)
  }


}
