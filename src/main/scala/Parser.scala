import Token._
import Error._
import SynTax.SynTaxState._
import StoreClass._
import TypeCode._

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

  def symPush(value: TypeCode.Value, sccType: SccType, i: Int, i1: Int) = ???

  def parameterTypeList(t:SccType, fc:Token.Value): Token.Value = {
    var funcCall: Token.Value = null
    var sccType:SccType = SccType(lexer.token,Symbol())
    var plast:Symbol = Symbol()
    var first = Symbol()
    lexer.getToken()
    var flag = true
    while (lexer.token != TK_CLOSEPA && flag) {
      if (typeSpecifier(sccType) == 0)
        error("invalid identifier", lexer)
      var tempToken = declarator(sccType,0)
      var s = symPush(| (tempToken,SC_PARAMS),sccType,0,0)
      //*plast = s;plast = &s -> next
      if (lexer.token == TK_CLOSEPA)
        flag = false
      else {
        skip(TK_COMMA, lexer)
        if (lexer.token == TK_ELLIPSIS) {
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
    val tempS = symPush(SC_ANOM,sccType,funcCall,0)
    tempS.next = first
    sccType.t = T_FUNC
    sccType.symbol = tempS
  }

  def directDeclaratorPostfix(t:SccType,fc:Token.Value): Unit = {
    lexer.token match {
      case TK_OPENPA =>
        parameterTypeList(t,fc)
      case TK_OPENBR =>
        lexer.getToken()
        lexer.token match {
          case TK_CINT =>
            lexer.getToken()
          //            n = tkvalue
          case _ =>
        }
        skip(TK_CLOSEBR, lexer)
        directDeclaratorPostfix(t,fc)
        val s = symPush(SC_ANOM,t,0,n)
        t.t = | (T_ARRAY,T_PTR)
        t.symbol = s
      case _ =>
    }
  }

  def directDeclarator(t:SccType,fc:Token.Value):Token.Value = {
    var vt = lexer.token
    lexer.token match {
      case x if x >= TK_IDENT =>
        lexer.getToken()
      case _ =>
        expect("identifier", lexer)
    }
    directDeclaratorPostfix(t,fc)
    vt
  }

  def declarator(t:SccType,forceAlign:Int) = {
    while (lexer.token == TK_STAR)
      lexer.getToken()
    var fc = functionCallingConvention()
    if(forceAlign != 0)
      structMemberAlignment(forceAlign)
    directDeclarator(t,fc)

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

  def argumentExpressionList(): Unit = {
    lexer.getToken()
    if (lexer.token != TK_CLOSEPA) {
      var flag = true
      while (flag) {
        assignmentExpression()
        if (lexer.token == TK_CLOSEPA)
          flag = false
        else
          skip(TK_COMMA, lexer)
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
          //          lexer.token |= SC_MEMBER
          lexer.getToken()
        case TK_OPENBR =>
          lexer.getToken()
          expression()
          skip(TK_CLOSEBR, lexer)
        case TK_OPENPA =>
          argumentExpressionList()
        case _ =>
          flag = false
      }
    }
  }

  def sizeofExpression() = {
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

  def initializer(t:SccType) =
    t match {
      case x if &(x.t,T_ARRAY) =>
        lexer.getToken()
      case _ =>
        assignmentExpression()
    }


  def externalDeclaration(l: StoreClass.Value): Unit = {
    var btype:SccType = SccType(1,Symbol())
    if (typeSpecifier(btype) == 0)
      expect("<type>", lexer)

    lexer.token match {
      case x if btype.t == T_STRUCT && x == TK_SEMICOLON =>
        syntaxState = SNTX_LF_HT
        lexer.getToken()
      case _ =>
        var flag = true
        val tempType = btype
        var v = lexer.token
        while (flag) {
          v = declarator(tempType,null)
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

  def typeSpecifier(t:SccType) = {
    var typeCode = T_VOID
    var typeFound = 0
    lexer.token match {
      case KW_CHAR =>
        typeCode = T_CHAR
        typeFound = 1
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_SHORT =>
        typeCode = T_SHORT
        typeFound = 1
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_VOID =>
        typeFound = 1
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_INT =>
        typeCode = T_INT
        typeFound = 1
        syntaxState = SNTX_SP
        lexer.getToken()
      case KW_STRUCT =>
        typeCode = T_STRUCT
        typeFound = 1
        syntaxState = SNTX_SP
        structSpecifier()
      case _ => error("error", lexer)
    }
    t.t = typeCode
    typeFound
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
      //      if (lexer.token != TK_CLOSEBR &&  lexer.token != TK_SEMICOLON && lexer.token != TK_CLOSEPA) {
      //        lexer.getToken()
      //      }
      else
        lexer.getToken()
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
    val s = localSymStack.stackGetTop()
    syntaxState = SNTX_LF_HT
    syntaxLevel += 1
    lexer.getToken()

    while (lexer.token != TK_END) {
      if (isTypeSpecifier(lexer.token))
        externalDeclaration(SC_LOCAL)
      else{
        statement()
      }
    }
    syntaxState = SNTX_LF_HT
    symPop(localSymStack,s)
    lexer.getToken()
  }

  def symDirectPush(localSymStack: Any, SC_ANOM: StoreClass.Value, SccType: SccType.type, i: Int) = ???

  def symPop(localSymStack: Any, value: Null) = ???

  def funcBody(sym:Symbol) = {
    symDirectPush(localSymStack,SC_ANOM,SccType,0)
    compoundStatement(null,null)
    symPop(localSymStack,null)
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


