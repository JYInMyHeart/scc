import ErrorLevel.ErrorLevel
import WorkStage.WorkStage

object Error {
  def handleException(stage: WorkStage, level: ErrorLevel, fmt: String, lexer: Lexer) = {
    if (stage == WorkStage.STAGE_COMPILE) {
      if (level == ErrorLevel.LEVEL_WARNING)
        println(s"${lexer.fileName} (line ${lexer.lineNum}) compiler warning:${fmt}")
      else {
        println(s"${lexer.fileName} (line ${lexer.lineNum}) compiler error:${fmt}")
        System.exit(-1)
      }
    } else {
      println(s"link error ${fmt}")
      System.exit(-1)
    }
  }

  def warning(fmt: String,  lexer: Lexer) =
    handleException(WorkStage.STAGE_COMPILE, ErrorLevel.LEVEL_WARNING, fmt, lexer)

  def error(fmt: String,  lexer: Lexer) =
    handleException(WorkStage.STAGE_COMPILE, ErrorLevel.LEVEL_ERROR, fmt, lexer)

  def expect(msg: String, lexer: Lexer) = error(s"miss $msg", lexer)

  def skip(c: Token.Value,  lexer: Lexer) = {
    if (lexer.token != c)
      error(s"<skip>miss ${getTkstr(c, Lexer.keyWords, lexer.fileName)}", lexer)
    lexer.getToken()
  }

  def getTkstr(v: Token.Value, tkTable: Map[Token.Value, String], sourceStr: String): String = {
    if (v.id > tkTable.size)
      null
    else if (v >= Token.TK_CINT && v <= Token.TK_CSTR)
      sourceStr
    else
      tkTable(v)
  }

  def linkError(fmt: String,  lexer: Lexer) =
    handleException(WorkStage.STAGE_LINK, ErrorLevel.LEVEL_ERROR, fmt, lexer)


}


object ErrorLevel extends Enumeration {
  type ErrorLevel = Value
  val
  LEVEL_WARNING,
  LEVEL_ERROR
  = Value
}

object WorkStage extends Enumeration {
  type WorkStage = Value
  val
  STAGE_COMPILE,
  STAGE_LINK
  = Value
}
