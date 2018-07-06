import ErrorLevel.ErrorLevel
import WorkStage.WorkStage

object Error {
  def handleException(stage: WorkStage, level: ErrorLevel, fmt: String, fileName: String) = {
    if (stage == WorkStage.STAGE_COMPILE) {
      if (level == ErrorLevel.LEVEL_WARNING)
        println(s"$fileName (line ${lineNum}) compiler warning:${buf}")
      else {
        println(s"$fileName (line ${lineNum}) compiler error:${buf}")
        System.exit(-1)
      }
    } else {
      println(s"link error ${buf}")
      System.exit(-1)
    }
  }

  def warning(fmt: String, fileName: String) =
    handleException(WorkStage.STAGE_COMPILE, ErrorLevel.LEVEL_WARNING, fmt, fileName)

  def error(fmt: String, fileName: String) =
    handleException(WorkStage.STAGE_COMPILE, ErrorLevel.LEVEL_ERROR, fmt, fileName)

  def expect(msg:String,fileName:String) = error(s"miss $msg",fileName)

  def skip(c:Int,fileName:String) = {
    if(token != c)
      error(s"miss $getTkstr(c)",fileName)
    getToken()
  }

  def getTkstr(v:Int,tkTable:Map[Int,String],sourceStr:String):String = {
    if(v > tkTable.size)
      return null
    else if(v >= Token.TK_CINT.id && v <= Token.TK_CSTR.id)
      return sourceStr
    else
      return tkTable.get(v).get
  }

  def linkError(fmt:String,fileName:String) =
    handleException(WorkStage.STAGE_LINK,ErrorLevel.LEVEL_ERROR,fmt,fileName)

  def getToken(ch:Char,fileName:String) = {
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
        if(ch == '>'){
          token = Token.TK_POINTSTO
          getCh()
        }else
          token = Token.TK_MINUS
      case '/' =>
        token = Token.TK_DIVIDE
        getCh()
      case '%' =>
        token = Token.MOD
        getCh()
      case '=' =>
        getCh()
        if(ch == '='){
          token = Token.TK_EQ
          getCh()
        }else
          token = Token.TK_ASSIGN
      case '!' =>
        getCh()
        if(ch == '!'){
          token = Token.TK_NEQ
          getCh()
        }else
          error("unsupported !",fileName)
      case '<' =>
        getCh()
        if(ch == '='){
          token = Token.TK_LEQ
          getCh()
        }else
          token = Token.TK_LT
      case '>' =>
        getCh()
        if(ch == '='){
          token = Token.TK_GEQ
          getCh()
        }else
          token = Token.TK_GT
    }
  }
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
