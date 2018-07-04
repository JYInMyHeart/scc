import java.io.File

import scala.io.{Source, StdIn}

object Lexer {
  def readSourceFile(file:File):String = {
    Source.fromFile(file).getLines().reduce(_+_)
  }
}
