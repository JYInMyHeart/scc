import java.io.{File, PushbackInputStream}

import Lexer.readSourceFile
import Token._
object Main {
  def main(args: Array[String]): Unit = {
    val fileName = "./src/main/scala/test.txt"
    val fin: PushbackInputStream = readSourceFile(new File(fileName))
    val lexer = new Lexer(token = TK_MOD, 1, ' ', fin, 0, fileName)
    implicit val p = new Parser(SynTax.SynTaxState.SNTX_NULL,0,lexer)
    lexer.getToken()
    val t = Ast(TranslationUnit(p.translationUnit(),TK_EOF))
    println()
    println(s"lineCount= ${lexer.lineNum}")


  }

}
