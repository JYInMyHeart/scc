

object Vistor {
  def visit(ast: Ast): Unit = {
    visit(Option.apply(ast.translationUnit))
  }

  def visit(t: Option[TranslationUnit]):Unit = {
    t match {
      case Some(tr) =>
        tr match {
          case TranslationUnit(e,eo) =>
            Option.apply(e) match {
              case Some(x) if x.nonEmpty => visitExternDeclaration(Some(x))
              case None => println("need externDecls")
            }
          case _ => println("need eof")
        }
      case None => println("need translationUnit")
    }
  }

  def visitExternDeclaration(t:Option[List[ExternDeclaration]]): Unit = {
    t match {
      case Some(x) =>
      case None => println("sth error")
    }
  }

}
