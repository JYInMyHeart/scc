import java.io.File

import Lexer._
object Main {
  def main(args: Array[String]): Unit = {
    val fileName = "/home/alex/IdeaProjects/scc/src/main/scala/test.txt"
    val fin = readSourceFile(new File(fileName))
    val lexer = new Lexer(token = Token.TK_MOD,1,' ',fin,0,fileName)
    while(lexer.token != Token.TK_EOF){
      lexer.getToken()
    }


  }

}
