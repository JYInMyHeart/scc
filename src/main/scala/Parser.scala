import Token._
import Error._
import SynTax.SynTaxState._
import StoreClass._

class Parser(var syntaxState: SynTaxState,
             var syntaxLevel: Int,
             val lexer: Lexer) {


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

  def parameterTypeList(): ParameterTypeList = {
    var funcCall: Token.Value = null
    var comma: Token.Value = null
    var ellopsis: Token.Value = null
    var declarators: List[Declarator] = List()
    var typeSpecifiers: List[Token.Value] = List()
    lexer.getToken()
    var flag = true
    while (lexer.token != TK_CLOSEPA && flag) {
      typeSpecifiers = typeSpecifiers :+ lexer.token
      if (typeSpecifier() == 0)
        error("invalid identifier", lexer)
      declarators = declarators :+ declarator()
      if (lexer.token == TK_CLOSEPA)
        flag = false
      else {
        skip(TK_COMMA, lexer)
        comma = TK_COMMA
        if (lexer.token == TK_ELLIPSIS) {
          ellopsis = TK_ELLIPSIS
          funcCall = KW_CDECL
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
    funcCall
    var parameterDecl: ParameterDecl = null
    val declarator1 = if (declarators.size > 0) {
      declarators.head
    } else null
    val typeSpecifier1 = if (typeSpecifiers.size > 0) typeSpecifiers.head else null
    val parameterDeclaration = ParameterDeclaration(TypeSpecifier(typeSpecifier1)())(declarator1)
    var parameterList: ParameterList = null
    if (comma != null) {
      var decls = for (d <- declarators.drop(0); t <- typeSpecifiers.drop(0)) yield {
        ParameterDecl(comma, ParameterDeclaration(TypeSpecifier(t)())(d))
      }
      parameterList = ParameterList(parameterDeclaration)(decls: _*)
    }
    ParameterTypeList(parameterList)(comma)(ellopsis)
  }

  def directDeclaratorPostfix(): DirectDeclaratorPostfix = {
    var open: Token.Value = null
    var close: Token.Value = null
    var cint: Token.Value = null
    var parameterTypeLists: ParameterTypeList = null
    lexer.token match {
      case TK_OPENPA =>
        open = TK_OPENPA
        parameterTypeLists = parameterTypeList()
      case TK_OPENBR =>
        open = TK_OPENBR
        lexer.getToken()
        lexer.token match {
          case TK_CINT =>
            cint = TK_CINT
            lexer.getToken()
          //            n = tkvalue
          case _ =>
        }
        skip(TK_CLOSEBR, lexer)
        close = TK_CLOSEBR
        directDeclaratorPostfix()
      case _ =>
    }
    var br: Br = null
    var pa: Pa = null
    open match {
      case TK_OPENPA =>
        pa = Pa(open)(parameterTypeLists)(close)
      case TK_OPENBR =>
        br = Br(open)(cint)(close)
      case _ =>
      //        error("nedd directDeclaratorPostfix", lexer)
    }
    if (br != null)
      DirectDeclaratorPostfix(br)()
    else
      DirectDeclaratorPostfix()(pa)
  }

  def directDeclarator() = {
    var ident: Token.Value = null
    lexer.token match {
      case x if x >= TK_IDENT =>
        ident = TK_IDENT
        lexer.getToken()
      case _ =>
        expect("identifier", lexer)
    }
    val directDeclaratorPostfixs = directDeclaratorPostfix()
    DirectDeclarator(ident, directDeclaratorPostfixs)
  }

  def declarator() = {
    var star: Token.Value = null
    while (lexer.token == TK_STAR) {
      star = TK_STAR
      lexer.getToken()
    }
    var fc = functionCallingConvention()
    val functionCallingConventions = FunctionCallingConvention(fc)
    val structMemberAlignments = structMemberAlignment()
    val directDeclarators = directDeclarator()
    Declarator(star)(functionCallingConventions)(structMemberAlignments)(directDeclarators)
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

  def argumentExpressionList(): PaArgueExpression = {
    var commas: List[Token.Value] = List()
    var assignmentExpressions: List[AssignmentExpression] = List()
    lexer.getToken()
    if (lexer.token != TK_CLOSEPA) {
      var flag = true
      while (flag) {
        assignmentExpressions = assignmentExpressions :+ assignmentExpression()
        if (lexer.token == TK_CLOSEPA)
          flag = false
        else {
          skip(TK_COMMA, lexer)
          commas = commas :+ TK_COMMA
        }
      }
    }
    skip(TK_CLOSEPA, lexer)
    val moreArguement = for (c <- commas; a <- assignmentExpressions.drop(0))
      yield MoreArguement(c, a)

    PaArgueExpression(TK_OPENPA)(
      ArguementExpressionList(assignmentExpressions.head)(moreArguement:_*))(TK_CLOSEPA)
  }

  def postfixExpression() = {
    val primaryExpressions = primaryExpression()
    var brExpressions: BrExpression = null
    var paExpression: PaArgueExpression = null
    var dotExpression: DotExpression = null
    var pointstoExpression: PointstoExpression = null
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
          //          lexer.token |= SC_MEMBER
          lexer.getToken()
        case TK_OPENBR =>
          lexer.getToken()
          val expressions = expression()
          skip(TK_CLOSEBR, lexer)
          brExpressions = BrExpression(TK_OPENBR)(expressions)(TK_CLOSEBR)
        case TK_OPENPA =>
          paExpression = argumentExpressionList()
        case _ =>
          flag = false
      }
    }
    PostfixExpression(primaryExpressions)(brExpressions)(paExpression)(dotExpression)(pointstoExpression)
  }

  def sizeofExpression() = {
    lexer.getToken()
    skip(TK_OPENPA, lexer)
    typeSpecifier()
    skip(TK_CLOSEPA, lexer)
  }

  def unaryExpression() = {
    var op: Token.Value = lexer.token
    var unaryExpressions: UnaryExpression = null
    var postfixExpressions: PostfixExpression = null
    var sizeofExpressions: SizeofExpression = null
    lexer.token match {
      case TK_AND =>
        lexer.getToken()
        unaryExpressions = unaryExpression()
      case TK_STAR =>
        lexer.getToken()
        unaryExpressions = unaryExpression()
      case TK_PLUS =>
        lexer.getToken()
        unaryExpressions = unaryExpression()
      case TK_MINUS =>
        lexer.getToken()
        unaryExpressions = unaryExpression()
      case KW_SIZEOF =>
        lexer.getToken()
        sizeofExpressions = sizeofExpression()
      case _ =>
        postfixExpressions = postfixExpression()
    }
    UnaryExpression(op)(unaryExpressions)(postfixExpressions)(sizeofExpressions)
  }

  def multiplicativeExpression() = {
    var op: Token.Value
    var nextUnaryExpression: UnaryExpression = null
    val unaryExpressions = unaryExpression()
    while (lexer.token == TK_STAR || lexer.token == TK_DIVIDE || lexer.token == TK_MOD) {
      lexer.getToken()
      nextUnaryExpression = unaryExpression()
    }
    MultplicativeExpression(unaryExpressions)(op)(nextUnaryExpression)
  }

  def additiveExpression() = {
    var op: Token.Value = null
    var nextMultiplicativeExpression: MultplicativeExpression = null
    val multplicativeExpressions = multiplicativeExpression()
    while (lexer.token == TK_PLUS || lexer.token == TK_MINUS) {
      op = lexer.token
      lexer.getToken()
      nextMultiplicativeExpression = multiplicativeExpression()
    }
    AdditiveExpression(multplicativeExpressions)(op)(nextMultiplicativeExpression)
  }

  def relationalExpression() = {
    var token: Token.Value = null
    val additiveExpressions = additiveExpression()
    var nextAdditiveExpression: AdditiveExpression = null
    while (lexer.token == TK_LT || lexer.token == TK_LEQ
      || lexer.token == TK_GT || lexer.token == TK_GEQ) {
      token = lexer.token
      lexer.getToken()
      nextAdditiveExpression = additiveExpression()
    }
    RelationExpression(additiveExpressions)(token)(nextAdditiveExpression)
  }

  def equalityExpression() = {
    var eq: Token.Value = null
    val relationExpressions = relationalExpression()
    var nextRelationExpression: RelationExpression = null
    while (lexer.token == TK_EQ || lexer.token == TK_NEQ) {
      eq = lexer.token
      lexer.getToken()
      nextRelationExpression = relationalExpression()
    }
    EqualityExpression(relationExpressions)(eq)(nextRelationExpression)
  }

  def assignmentExpression(): AssignmentExpression = {
    var assign: Token.Value = null
    var nextAssignmentExpr: AssignmentExpression = null
    val equalityExpressions = equalityExpression()
    if (lexer.token == TK_ASSIGN) {
      assign = TK_ASSIGN
      lexer.getToken()
      nextAssignmentExpr = assignmentExpression()
    }
    AssignmentExpression(equalityExpressions)(assign)(nextAssignmentExpr)
  }

  def initializer() = {
    assignmentExpression()
  }

  def externalDeclaration(l: StoreClass.Value): ExternDeclaration = {
    val typeSpecifiers = TypeSpecifier(lexer.token)()
    var declarators: Declarator = null
    var semicolon: Token.Value = null
    var comma: Token.Value = null

    if (typeSpecifier() == 0)
      expect("<type>", lexer)

    lexer.token match {
      case TK_SEMICOLON =>
        syntaxState = SNTX_LF_HT
        semicolon = lexer.token
        lexer.getToken()
      case _ =>
        var flag = true
        while (flag) {
          declarators = declarator()
          lexer.token match {
            case TK_BEGIN => {
              if (l == SC_LOCAL)
                error("unsupported internal declaration", lexer)
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
                  comma = TK_COMMA
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
    ExternDeclaration(typeSpecifiers)(semicolon)()(declarators)
  }

  def structDeclaration(): Unit = {
    typeSpecifier()
    var flag = true
    while (flag) {
      declarator()
      if (lexer.token == TK_SEMICOLON)
        flag = false
      else
        skip(TK_COMMA, lexer)
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

  def typeSpecifier() = {
    var typeFound = 0
    lexer.token match {
      case KW_CHAR =>
        typeFound = 1
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_SHORT =>
        typeFound = 1
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_VOID =>
        typeFound = 1
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_INT =>
        typeFound = 1
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_STRUCT =>
        typeFound = 1
        syntaxState = SNTX_SP
        structSpecifier()
      case _ => error("error", lexer)
    }
    typeFound
  }

  def translationUnit() = {
    var list: List[ExternDeclaration] = List()
    while (lexer.token != TK_EOF) {
      val externDeclaration = externalDeclaration(SC_GLOBAL)
      list = list :+ externDeclaration
    }
    list
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
    var align: Token.Value = null
    var openpa: Token.Value = null
    var cint: Token.Value = null
    var closepa: Token.Value = null
    lexer.token match {
      case KW_ALIGN => {
        align = KW_ALIGN
        lexer.getToken()
        openpa = TK_OPENPA
        skip(TK_OPENPA, lexer)
        lexer.token match {
          case TK_CINT =>
            cint = TK_CINT
            lexer.getToken()
          case _ =>
            expect("need const variable", lexer)
        }
        skip(TK_CLOSEPA, lexer)
        closepa = TK_CLOSEPA
      }
      case _ =>
    }
    StructMemberAlignment(align, openpa, cint, closepa)
  }

  def expression() = {
    var commas:List[Token.Value] = List()
    var assignExpr:List[AssignmentExpression] = List()
    var flag = true
    while (flag) {
      assignExpr = assignExpr :+ assignmentExpression()
      if (lexer.token != TK_COMMA)
        flag = false
      else{
        commas = commas :+ TK_COMMA
        lexer.getToken()
      }
    }
    val arguements = for(c <- commas;p <- assignExpr.drop(0))
      yield MoreArguement(c,p)
    Expression(assignExpr.head)(arguements:_*)
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
    statement()
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

  def compoundStatement() = {
    syntaxState = SNTX_LF_HT
    syntaxLevel += 1
    lexer.getToken()

    while (lexer.token != TK_END) {
      if (isTypeSpecifier(lexer.token))
        externalDeclaration(SC_LOCAL)
      else {
        statement()
      }
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


