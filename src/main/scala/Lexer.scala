import java.io.File

import Error.{error, warning}
import Lexer.EOF
import Token.Token

import scala.io.Source

class Lexer(var token: Token,
            var lineNum: Int,
            var ch: Char,
            val fin: String,
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
    tkWords += count -> temp
  }

  def parseNum() = {
    getCh()
  }

  def parseString(sep: Char): String = {
    var c: Char = ' '
    var str: String = sep.toString
    getCh()
    var flag = true
    while (true) {
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
      case x if x >= '0' && x <= '9' => {
        parseNum()
        token = Token.TK_CINT
      }
      case '-' =>
        getCh()
        if (ch == '>') {
          token = Token.TK_POINTSTO
          getCh()
        } else
          token = Token.TK_MINUS
      case '/' =>
        token = Token.TK_DIVIDE
        getCh()
      case '%' =>
        token = Token.TK_MOD
        getCh()
      case '=' =>
        getCh()
        if (ch == '=') {
          token = Token.TK_EQ
          getCh()
        } else
          token = Token.TK_ASSIGN
      case '!' =>
        getCh()
        if (ch == '!') {
          token = Token.TK_NEQ
          getCh()
        } else
          error("unsupported !", fileName, this)
      case '<' =>
        getCh()
        if (ch == '=') {
          token = Token.TK_LEQ
          getCh()
        } else
          token = Token.TK_LT
      case '>' =>
        getCh()
        if (ch == '=') {
          token = Token.TK_GEQ
          getCh()
        } else
          token = Token.TK_GT
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
      case '&' =>
        token = Token.TK_AND
        getCh()
      case ';' =>
        token = Token.TK_SEMICOLON
        getCh()
      case ']' =>
        token = Token.TK_CLOSEBR
        getCh()
      case '}' =>
        token = Token.TK_END
        getCh()
      case ')' =>
        token = Token.TK_CLOSEPA
        getCh()
      case '{' =>
        token = Token.TK_BEGIN
        getCh()
      case '[' =>
        token = Token.TK_OPENBR
        getCh()
      case ',' =>
        token = Token.TK_COMMA
        getCh()
      case '(' =>
        token = Token.TK_OPENPA
        getCh()
      case '*' =>
        token = Token.TK_STAR
        getCh()
      case '\'' =>
        parseString(ch)
        token = Token.TK_CCHAR
      //        tkValue = tkstr.data
      case '\"' =>
        parseString(ch)
        token = Token.TK_CSTR
      case EOF =>
        token = Token.TK_EOF
      case _ =>
        error("unknown characters", fileName, this)
        getCh()
    }
  }

  def getCh() = {
    ch = fin(count)
    count += 1
  }

  def ungetC(c: Char, str: String) = {
    count = count - 1
    ch = fin(count)
  }

  def preprocess() = {
    var flag = true
    while (flag) {
      if (ch == ' ' || ch == '\t' || ch == '\r')
        skipWhiteSpace()
      else if (ch == '/') {
        getCh()
        if (ch == '*') {
          parseComment()
        } else {
          ungetC(ch, fin)
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
      if (ch == '\n') {
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
    while (ch == ' ' || ch == '\t' || ch == '\r') {
      if (ch == '\r') {
        getCh()
        if (ch != '\n')
          flag = false
        lineNum += 1
      }
      println(ch)
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

  var tkWords = Map[Int, String]()

  def readSourceFile(file: File): String = {
    Source.fromFile(file).getLines().reduce(_ + _)
  }


  def isNodigit(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  def isDigit(c: Char) = c >= '0' && c <= '9'


}
