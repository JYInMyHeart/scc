import Token._
import Error._
import SynTax.SynTaxState._
class Parser(var syntaxState:SynTaxState,
             var syntaxLevel:Int,
             val lexer: Lexer) {

  def funcBody() = ???

  def parameterTypeList() = ???

  def directDeclaratorPostfix():Unit = {
    lexer.token match {
      case TK_OPENPA =>
        parameterTypeList()
      case TK_OPENBR =>
        lexer.getToken()
        lexer.token match {
          case TK_CINT =>
            lexer.getToken()
//            n = tkvalue
        }
        skip(TK_CLOSEBR,lexer)
        directDeclaratorPostfix()
    }
  }

  def directDeclarator() = {
    lexer.token match {
      case TK_IDENT =>
        lexer.getToken()
      case _ =>
        expect("identifier",lexer)
    }
    directDeclaratorPostfix()
  }

  def declarator() = {
    while(lexer.token == TK_STAR)
      lexer.getToken()
    var fc = functionCallingConvention()
    structMemberAlignment
    directDeclarator()

  }

  def initializer() = ???

  def externalDeclaration(l: Int): Unit = {
    if(typeSpecifier() == 0)
      expect("<type>",lexer)

    lexer.token match {
      case TK_SEMICOLON =>
        lexer.getToken()
      case _ => {
        var flag = true
        while(flag){
          declarator()
          lexer.token match {
            case TK_BEGIN => {
              if(l == SC_LOCAL)
                error("unsupported internal declaration",lexer)
              funcBody()
              flag = false
            }
            case TK_ASSIGN =>
              lexer.getToken()
              initializer()
            case TK_COMMA =>
              lexer.getToken()
            case _ =>
              syntaxState = SNTX_LF_HT
              skip(TK_SEMICOLON,lexer)
              flag = false
          }
        }
      }
    }
  }

  def structDeclaration() = {
    typeSpecifier()
    var flag = true
    while(flag){
      declarator()
      if(lexer.token == TK_SEMICOLON)
        flag = false
      skip(TK_COMMA,lexer)
    }
    syntaxState = SNTX_LF_HT
    skip(TK_SEMICOLON,lexer)
  }

  def structDeclarationList() = {
    syntaxState = SNTX_LF_HT
    syntaxLevel += 1
    lexer.getToken()
    while(lexer.token != TK_END)
      structDeclaration()
    skip(TK_END,lexer)
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
    syntaxIndet()
    if(v < TK_IDENT)
      expect("struct name",lexer)
    if(lexer.token == TK_BEGIN)
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
        lexer.getToken()
      case _ => error("error",lexer,lexer)
    }
    typeFound
  }

  def translationUnit() = {
    while(lexer.token != TK_EOF){
      externalDeclaration(SC_GLOBAL)
    }
  }

  def functionCallingConvention()= {
    var fc = KW_CDECL
    if(lexer.token == KW_CDECL || lexer.token == KW_STDCALL){
      fc = lexer.token
      syntaxState = SNTX_SP
      lexer.getToken()
    }
    fc
  }

  def structMemberAlignment = {
    lexer.token match {
      case KW_ALIGN => {
        lexer.getToken()
        skip(TK_OPENPA)
        lexer.token  match {
          case TK_CINT =>
            lexer.getToken()
          case _ =>
            expect("need const variable",lexer)
        }
        skip(TK_CLOSEPA,lexer)
      }
    }
  }
}
