import ErrorLevel.ErrorLevel
import WorkStage.WorkStage

object Error {
  def handleException(stage: WorkStage, level: ErrorLevel, fmt: String, fileName: String, lexer: Lexer) = {
    if (stage == WorkStage.STAGE_COMPILE) {
      if (level == ErrorLevel.LEVEL_WARNING)
        println(s"$fileName (line ${lexer.lineNum}) compiler warning:${fmt}")
      else {
        println(s"$fileName (line ${lexer.lineNum}) compiler error:${fmt}")
        System.exit(-1)
      }
    } else {
      println(s"link error ${fmt}")
      System.exit(-1)
    }
  }

  def warning(fmt: String, fileName: String, lexer: Lexer) =
    handleException(WorkStage.STAGE_COMPILE, ErrorLevel.LEVEL_WARNING, fmt, fileName, lexer)

  def error(fmt: String, fileName: String, lexer: Lexer) =
    handleException(WorkStage.STAGE_COMPILE, ErrorLevel.LEVEL_ERROR, fmt, fileName, lexer)

  def expect(msg: String, fileName: String, lexer: Lexer) = error(s"miss $msg", fileName, lexer)

  def skip(c: Token.Value, fileName: String, lexer: Lexer) = {
    if (lexer.token != c)
      error(s"miss ${getTkstr(c, Lexer.keyWords, fileName)}", fileName, lexer)
    lexer.getToken()
  }

  def getTkstr(v: Token.Value, tkTable: Map[Token.Value, String], sourceStr: String): String = {
    if (v.id > tkTable.size)
      return null
    else if (v >= Token.TK_CINT && v <= Token.TK_CSTR)
      return sourceStr
    else
      return tkTable.get(v).get
  }

  def linkError(fmt: String, fileName: String, lexer: Lexer) =
    handleException(WorkStage.STAGE_LINK, ErrorLevel.LEVEL_ERROR, fmt, fileName, lexer)


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
