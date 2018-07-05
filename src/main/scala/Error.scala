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
