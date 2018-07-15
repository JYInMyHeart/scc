import Token._
import Error._
import SynTax.SynTaxState._
import StoreClass._

class Parser(var syntaxState: SynTaxState,
             var syntaxLevel: Int,
             val lexer: Lexer) {


  def printTab(syntaxLevel: Int) = {
    for (i <- 0 to syntaxLevel)
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

  def parameterTypeList(): Token.Value = {
    var funcCall: Token.Value = null
    lexer.getToken()
    while (lexer.token != TK_CLOSEPA) {
      if (typeSpecifier() != 0)
        error("invalid identifier", lexer)
      declarator()
      if (lexer.token == TK_CLOSEPA)
        return funcCall
      skip(TK_COMMA, lexer)
      if (lexer.token == TK_ELLIPSIS) {
        funcCall = KW_CDECL
        lexer.getToken()
        return funcCall
      }
      funcCall
    }
    syntaxState = SNTX_DELAY
    skip(TK_CLOSEPA, lexer)
    if (lexer.token == TK_BEGIN)
      syntaxState = SNTX_LF_HT
    else
      syntaxState = SNTX_NULL
    syntaxIndent()
    funcCall
  }

  def directDeclaratorPostfix(): Unit = {
    lexer.token match {
      case TK_OPENPA =>
        parameterTypeList()
      case TK_OPENBR =>
        lexer.getToken()
        lexer.token match {
          case TK_CINT =>
            lexer.getToken()
          //            n = tkvalue
          case _ =>
        }
        skip(TK_CLOSEBR, lexer)
        directDeclaratorPostfix()
      case _ =>
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

  def declarator() = {
    while (lexer.token == TK_STAR)
      lexer.getToken()
    var fc = functionCallingConvention()
    structMemberAlignment()
    directDeclarator()

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

  def initializer() = {
    assignmentExpression()
  }

  def externalDeclaration(l: StoreClass.Value): Unit = {
    if (typeSpecifier() == 0)
      expect("<type>", lexer)

    lexer.token match {
      case TK_SEMICOLON =>
        lexer.getToken()
      case _ =>
        var flag = true
        while (flag) {
          declarator()
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
        structSpecifier()
        syntaxState = SNTX_SP
      case _ => error("error", lexer)
    }
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


