import java.io.{File, PushbackInputStream}
import Token._
import Lexer.readSourceFile

object Main {
  def main(args: Array[String]): Unit = {
    val fileName = "./src/main/scala/test.txt"
    val fin: PushbackInputStream = readSourceFile(new File(fileName))
    val lexer = new Lexer(token = TK_MOD, 1, ' ', fin, 0, fileName)
    while (lexer.ch.toShort != -1) {
      lexer.getToken()
    }
    println(s"lineCount= ${lexer.lineNum}")
    Lexer.colorToken
  }
}
