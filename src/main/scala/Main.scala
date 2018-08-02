import java.io.{File, PushbackInputStream}

import Lexer.readSourceFile
import Token._

import scala.collection.mutable
import scala.collection.mutable.HashSet
object Main {
  def main(args: Array[String]): Unit = {
    val fileName = "./src/main/scala/test.txt"
    val fin: PushbackInputStream = readSourceFile(new File(fileName))
    val lexer = new Lexer(token = TK_MOD, 1, ' ', fin, 0, fileName)
    implicit val p = new Parser(SynTax.SynTaxState.SNTX_NULL,0,lexer)

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
      Map[String,Entity](),
      List[LocalScope]()
    )
    val ast = new MyAST(declarations,scope)
    lexer.getToken()
    val t = Ast(TranslationUnit(p.translationUnit(ast),TK_EOF))
    println()
    println(s"lineCount= ${lexer.lineNum}")


  }

}
