import java.io.{File, PushbackInputStream}
import Token._
import Lexer.readSourceFile

object Main {
  def main(args: Array[String]): Unit = {
    val fileName = "./src/main/scala/test.txt"
    val fin: PushbackInputStream = readSourceFile(new File(fileName))
    val lexer = new Lexer(token = TK_MOD, 1, ' ', fin, 0, fileName)
    implicit val p = new Parser(SynTax.SynTaxState.SNTX_NULL,0,lexer)
    lexer.getToken()
    val t = p.translationUnit()
    println()
    println(s"lineCount= ${lexer.lineNum}")
    println(t(1).functionDefinition.head.declarator )
  }
}
