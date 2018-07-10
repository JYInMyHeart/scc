import java.io.{File, PushbackInputStream}

import Lexer.readSourceFile

object Main {
  def main(args: Array[String]): Unit = {
    val fileName = "./src/main/scala/test.txt"
    val fin: PushbackInputStream = readSourceFile(new File(fileName))
    val lexer = new Lexer(token = Token.TK_MOD, 1, ' ', fin, 0, fileName)
    var tokens: List[Token.Value] = List()
    var tt: List[(Token.Value, String)] = List()
    while (lexer.ch.toShort != -1) {
      lexer.getToken()
      tokens ++= List(lexer.token)
    }
    println(s"lineCount= ${lexer.lineNum}")
    Lexer.colorToken

    //    tokens.map(x => if (Lexer.keyWords.contains(x)) Lexer.keyWords(x)).foreach(println)
    //    import io.AnsiColor._
    //    println(s"${BLUE}${BOLD}Hello 1979!${RESET}")
  }
}
