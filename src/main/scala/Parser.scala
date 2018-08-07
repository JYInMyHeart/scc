import Error._
import StoreClass._
import SynTax.SynTaxState._
import Token._
import Lexer.tkWords
import Lexer.keyWords
class Parser(var syntaxState: SynTaxState,
             var syntaxLevel: Int,
             val lexer: Lexer,
             var ast: MyAST) {


  implicit val p = this

  def printTab(syntaxLevel: Int) = {
    for (i <- 0 until syntaxLevel)
      print("\t")
  }

  def syntaxIndent() = {
    syntaxState match {
      case SNTX_NULL =>
        print(Lexer.color(Lexer.tkWords.head))
      case SNTX_SP =>
        print(" ")
        print(Lexer.color(Lexer.tkWords.head))
      case SNTX_LF_HT =>
        if (lexer.token == TK_END)
          syntaxLevel -= 1
        print("\n")
        printTab(syntaxLevel)
        print(Lexer.color(Lexer.tkWords.head))
      case SNTX_DELAY =>
    }
    syntaxState = SNTX_NULL
  }

  def parameterTypeList() = {
    lexer.getToken()
    var flag = true
    while (lexer.token != TK_CLOSEPA && flag) {
      typeSpecifier()
      declarator()
      if (lexer.token == TK_CLOSEPA)
        flag = false
      else {
        skip(TK_COMMA, lexer)
        if (lexer.token == TK_ELLIPSIS) {
          lexer.getToken()
          flag = false
        }
      }
    }
    syntaxState = SNTX_DELAY
    skip(TK_CLOSEPA, lexer)
    if (lexer.token == TK_BEGIN)
      syntaxState = SNTX_LF_HT
    else
      syntaxState = SNTX_NULL
    syntaxIndent()
  }

  def directDeclaratorPostfix(): TypeCode.Value = {
    lexer.token match {
      case TK_OPENPA =>
        parameterTypeList()
        TypeCode.T_FUNC
      case TK_OPENBR =>
        lexer.getToken()
        lexer.token match {
          case TK_CINT =>
            lexer.getToken()
          case _ =>
        }
        skip(TK_CLOSEBR, lexer)
        TypeCode.T_ARRAY
//        directDeclaratorPostfix()
      case _ => TypeCode.T_VOID
    }
  }

  def directDeclarator() = {
    lexer.token match {
      case x if x >= TK_IDENT =>
        lexer.getToken()
      case _ =>
        expect("identifier", lexer)
    }
    directDeclaratorPostfix()
  }

  def declarator(): (String, TypeCode.Value) = {
    var star: String = ""
    while (lexer.token == TK_STAR) {
      star += keyWords(TK_STAR)
      lexer.getToken()
    }
    star += tkWords.head._3
    functionCallingConvention()
    structMemberAlignment()
    (star,directDeclarator())

  }

  def primaryExpression() = {
    lexer.token match {
      case TK_CINT =>
        lexer.getToken()
      case TK_CCHAR =>
        lexer.getToken()
      case TK_CSTR =>
        lexer.getToken()
      case TK_OPENPA =>
        lexer.getToken()
        expression()
        skip(TK_CLOSEPA, lexer)
      case _ =>
        val t = lexer.token
        lexer.getToken()
        if (t < TK_IDENT)
          expect("const characters", lexer)
    }
  }

  def argumentExpressionList() = {

    lexer.getToken()
    if (lexer.token != TK_CLOSEPA) {
      var flag = true
      while (flag) {
        assignmentExpression()
        if (lexer.token == TK_CLOSEPA)
          flag = false
        else {
          skip(TK_COMMA, lexer)
        }
      }
    }
    skip(TK_CLOSEPA, lexer)

  }

  def postfixExpression() = {
    primaryExpression()
    var flag = true
    while (flag) {
      lexer.token match {
        case TK_DOT =>
          lexer.getToken()
          lexer.token = TK_IDENT
          lexer.getToken()
        case TK_POINTSTO =>
          lexer.getToken()
          lexer.token = TK_IDENT
          lexer.getToken()
        case TK_OPENBR =>
          lexer.getToken()
          val expressions = expression()
          skip(TK_CLOSEBR, lexer)
          expressions
        case TK_OPENPA =>
          argumentExpressionList()
        case _ =>
          flag = false
      }
    }
  }

  def sizeofExpression() = {
    val sizeof = Token.KW_SIZEOF
    lexer.getToken()
    skip(TK_OPENPA, lexer)
    typeSpecifier()
    skip(TK_CLOSEPA, lexer)
  }

  def unaryExpression(): Unit = {

    lexer.token match {
      case TK_AND =>
        lexer.getToken()
        unaryExpression()
      case TK_STAR =>
        lexer.getToken()
        unaryExpression()
      case TK_PLUS =>
        lexer.getToken()
        unaryExpression()
      case TK_MINUS =>
        lexer.getToken()
        unaryExpression()
      case KW_SIZEOF =>
        lexer.getToken()
        sizeofExpression()
      case _ =>
        postfixExpression()
    }
  }

  def multiplicativeExpression() = {

    unaryExpression()
    while (lexer.token == TK_STAR || lexer.token == TK_DIVIDE || lexer.token == TK_MOD) {
      lexer.getToken()
      unaryExpression()
    }

  }

  def additiveExpression() = {
    multiplicativeExpression()
    while (lexer.token == TK_PLUS || lexer.token == TK_MINUS) {
      lexer.getToken()
      multiplicativeExpression()
    }
  }

  def relationalExpression() = {
    additiveExpression()
    while (lexer.token == TK_LT || lexer.token == TK_LEQ
      || lexer.token == TK_GT || lexer.token == TK_GEQ) {
      lexer.getToken()
      additiveExpression()
    }
  }

  def equalityExpression() = {
    relationalExpression()
    while (lexer.token == TK_EQ || lexer.token == TK_NEQ) {

      lexer.getToken()
      relationalExpression()
    }
  }

  def assignmentExpression(): Unit = {
    equalityExpression()
    if (lexer.token == TK_ASSIGN) {
      lexer.getToken()
      assignmentExpression()
    }
  }

  def initializer() = {
    assignmentExpression()
  }

  //  def getTypeCode(typeSpecifiers: TypeSpecifier):TypeCode.Value = {
  //    typeSpecifiers.t match {
  //      case KW_CHAR =>
  //        T_CHAR
  //      case KW_SHORT =>
  //        T_SHORT
  //      case KW_VOID =>
  //        T_VOID
  //      case KW_INT =>
  //        T_INT
  //      case KW_STRUCT =>
  //        T_STRUCT
  //      case _ => T_FUNC
  //    }
  //  }

  def getTypeNode(res:(String, TypeCode.Value),typeSpecifiers:Token.Value) = {
    def getIdentType(t:TypeCode.Value) = t match {
      case TypeCode.T_VOID =>
        new TypeNode(keyWords(typeSpecifiers))
      case TypeCode.T_ARRAY =>
         new TypeNode(keyWords(typeSpecifiers) + "[]")
      case _ =>
        new TypeNode(keyWords(typeSpecifiers))
    }
    getIdentType(res._2)
  }
  def externalDeclaration(l: StoreClass.Value): List[DefineVaribale] = {
    val typeSpecifiers = typeSpecifier()
    var typeNode:TypeNode = null
    var localScope:LocalScope = null
    var ident:String = ""
    var defvars = List[DefineVaribale]()
    lexer.token match {
      case TK_SEMICOLON =>
        syntaxState = SNTX_LF_HT
        lexer.getToken()
      case _ =>
        var flag = true
        while (flag) {
          val res = declarator()
          ident = res._1

          typeNode = getTypeNode(res,typeSpecifiers)

          if(l == SC_GLOBAL){
            ast.declarations.typedefs += new TypedefNode(typeNode,ident,typeNode)
            ast.scope.entities += ident -> new Entity(ident,false,typeNode)
          }else{
            ast.scope.children.head.varaiables +=
              ident -> new DefineVaribale(null,0,ident,true,typeNode)
          }

          lexer.token match {
            case TK_BEGIN => {
              if (l == SC_LOCAL)
                error("unsupported internal declaration", lexer)
              localScope = new LocalScope(ast.scope,Map[String,DefineVaribale](),List[LocalScope]())
              ast.scope.children +:= localScope
              val blockNode: BlockNode = new BlockNode(List[DefineVaribale](),List[StmtNode](),null)
              ast.declarations.defuns += new DefinedFunction(ident,true,typeNode,null,blockNode)
              funcBody()
              flag = false
            }
            case _ => {
              if (lexer.token == TK_ASSIGN) {
                lexer.getToken()
                initializer()
              }
              lexer.token match {
                case TK_COMMA =>
                  lexer.getToken()
                case _ =>
                  syntaxState = SNTX_LF_HT
                  skip(TK_SEMICOLON, lexer)
                  flag = false
              }
            }
          }
        }
    }
//    ast.declarations.typedefs += new TypedefNode(typeNode,ident,typeNode)
//    if(l == SC_GLOBAL)
//      ast.scope.entities += ident -> new Entity(ident,false,typeNode)
//    else{
//      ast.scope.children.head.varaiables +=
//        ident -> new DefineVaribale(null,0,ident,true,typeNode)
//    }
    defvars
  }

  def structDeclaration() = {
    typeSpecifier()
    var flag = true
    while (flag) {
      declarator()
      if (lexer.token == TK_SEMICOLON)
        flag = false
      else {
        skip(TK_COMMA, lexer)
      }
    }
    syntaxState = SNTX_LF_HT
    skip(TK_SEMICOLON, lexer)
  }

  def structDeclarationList() = {
    syntaxState = SNTX_LF_HT
    syntaxLevel += 1
    lexer.getToken()
    while (lexer.token != TK_END)
      structDeclaration()
    skip(TK_END, lexer)

  }

  def structSpecifier() = {
    lexer.getToken()
    var v = lexer.token
    syntaxState = SNTX_DELAY
    lexer.getToken()
    lexer.token match {
      case TK_BEGIN =>
        syntaxState = SNTX_LF_HT
      case TK_CLOSEPA =>
        syntaxState = SNTX_SP
      case _ =>
        syntaxState = SNTX_SP
    }
    syntaxIndent()
    if (v < TK_IDENT)
      expect("struct name", lexer)
    if (lexer.token == TK_BEGIN)
      structDeclarationList()
  }

  def typeSpecifier(): Token = {
    val t = lexer.token
    var typeFound = 0
    lexer.token match {
      case KW_CHAR =>
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_SHORT =>
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_VOID =>
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_INT =>
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_STRUCT =>
        syntaxState = SNTX_SP
        structSpecifier()
      case _ => error("error need <type>", lexer)
    }
    t
  }

  def translationUnit() = {
     while (lexer.token != TK_EOF) {
      externalDeclaration(SC_GLOBAL)
    }
  }

  def functionCallingConvention() = {
    var fc = KW_CDECL
    if (lexer.token == KW_CDECL || lexer.token == KW_STDCALL) {
      fc = lexer.token
      syntaxState = SNTX_SP
      lexer.getToken()
    }
    fc
  }

  def structMemberAlignment() = {
    lexer.token match {
      case KW_ALIGN => {
        lexer.getToken()
        skip(TK_OPENPA, lexer)
        lexer.token match {
          case TK_CINT =>
            lexer.getToken()
          case _ =>
            expect("need const variable", lexer)
        }
        skip(TK_CLOSEPA, lexer)
      }
      case _ =>
    }
  }

  def expression() = {
    var flag = true
    while (flag) {
      assignmentExpression()
      if (lexer.token != TK_COMMA)
        flag = false
      else {
        lexer.getToken()
      }
    }
  }

  def ifStatement() = {
    syntaxState = SNTX_SP
    lexer.getToken()
    skip(TK_OPENPA, lexer)
    expression()
    syntaxState = SNTX_LF_HT
    skip(TK_CLOSEPA, lexer)
    statement()
    if (lexer.token == KW_ELSE) {
      syntaxState = SNTX_LF_HT
      lexer.getToken()
      statement()
    }
  }

  def returnStatement() = {
    syntaxState = SNTX_DELAY
    lexer.getToken()
    lexer.token match {
      case TK_SEMICOLON =>
        syntaxState = SNTX_NULL
      case _ => syntaxState = SNTX_SP
    }
    syntaxIndent()
    if (lexer.token != TK_SEMICOLON)
      expression()
    syntaxState = SNTX_LF_HT
    skip(TK_SEMICOLON, lexer)
  }

  def breakStatement() = {
    lexer.getToken()
    syntaxState = SNTX_LF_HT
    skip(TK_SEMICOLON, lexer)
  }

  def continueStatement() = {
    lexer.getToken()
    syntaxState = SNTX_LF_HT
    skip(TK_SEMICOLON, lexer)
  }

  def forStatement() = {
    lexer.getToken()
    skip(TK_OPENPA, lexer)
    if (lexer.token != TK_SEMICOLON)
      expression()
    skip(TK_SEMICOLON, lexer)
    if (lexer.token != TK_SEMICOLON)
      expression()
    skip(TK_SEMICOLON, lexer)
    if (lexer.token != TK_CLOSEPA)
      expression()
    syntaxState = SNTX_LF_HT
    skip(TK_CLOSEPA, lexer)
    val stmt = statement()
  }

  def expressionStatement() = {
    lexer.token match {
      case TK_SEMICOLON =>
      case _ => expression()
    }
    syntaxState = SNTX_LF_HT
    skip(TK_SEMICOLON, lexer)
  }

  def isTypeSpecifier(token: Token): Boolean = {
    token match {
      case KW_CHAR => true
      case KW_SHORT => true
      case KW_INT => true
      case KW_VOID => true
      case KW_STRUCT => true
      case _ => false
    }
  }

  def compoundStatement(): Unit = {
    syntaxState = SNTX_LF_HT
    syntaxLevel += 1
    lexer.getToken()
    var defvars = List[DefineVaribale]()
    var stmts = List[StmtNode]()
    while (lexer.token != TK_END) {
      if (isTypeSpecifier(lexer.token))
        defvars ++= externalDeclaration(SC_LOCAL)
      else
        stmts :+= statement()
    }

    syntaxState = SNTX_LF_HT
    lexer.getToken()
  }

  def funcBody() = {
    compoundStatement()
  }

  def statement(): Unit = {
    lexer.token match {
      case TK_BEGIN =>
        compoundStatement()
      case KW_IF =>
        ifStatement()
      case KW_RETURN =>
        returnStatement()
      case KW_BREAK =>
        breakStatement()
      case KW_CONTINUE =>
        continueStatement()
      case KW_FOR =>
        forStatement()
      case _ =>
        expressionStatement()
    }
  }
}


