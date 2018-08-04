import java.io.{ByteArrayInputStream, File, PushbackInputStream}

import Lexer.readSourceFile
import Token._

object Main {
  def main(args: Array[String]): Unit = {
    val fileName = "./src/main/scala/test.txt"
    val fin: PushbackInputStream = readSourceFile(new File(fileName))
    val lexer = new Lexer(token = TK_MOD, 1, ' ', fin, 0, fileName)


    var declarations = new Declarations(
      Set[DefineVaribale](),
      Set[UndefinedVariable](),
      Set[DefinedFunction](),
      Set[UndefinedFunction](),
      Set[Constant](),
      Set[StructNode](),
      Set[TypedefNode]()
    )
    var scope = new TopScope(
      List[DefineVaribale](),
      Map[String, Entity](),
      List[LocalScope]()
    )
    val ast = new MyAST(declarations, scope)
    implicit val p = new Parser(SynTax.SynTaxState.SNTX_NULL, 0, lexer, ast)
    lexer.getToken()
    p.translationUnit()
    println()
    println(s"lineCount= ${lexer.lineNum}")


  }

}


