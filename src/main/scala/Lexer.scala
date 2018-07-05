import java.io.File

import scala.io.Source

object Lexer {
  val tkword: (Token.Value, String) = {
    Token.TK_PLUS -> "+"
    Token.TK_MINUS->"-"
    Token.TK_STAR->"*"
    Token.TK_DIVIDE->"/"
    Token.TK_MOD->"%"
    Token.TK_NEQ->"!="
    Token.TK_EQ->"=="
    Token.TK_LT->"<"
    Token.TK_LEQ->"<="
    Token.TK_GT->">"
    Token.TK_GEQ->">="
    Token.TK_ASSIGN->"="
    Token.TK_POINTSTO->"->"
    Token.TK_DOT->"."
    Token.TK_AND->"&"
    Token.TK_OPENPA->"("
    Token.TK_CLOSEPA->")"
    Token.TK_OPENBR->"["
    Token.TK_CLOSEBR->"]"
    Token.TK_BEGIN->"{"
    Token.TK_END->"}"
    Token.TK_SEMICOLON->";"
    Token.TK_COMMA->","
    Token.TK_ELLIPSIS->"..."
    Token.TK_EOF->"End_Of_File"
    Token.TK_CINT->"integer"
    Token.TK_CCHAR->"character"
    Token.TK_CSTR->"string"
    Token.KW_CHAR->"char"
    Token.KW_SHORT->"short"
    Token.KW_INT->"int"
    Token.KW_VOID->"void"
    Token.KW_STRUCT->"struct"
    Token.KW_IF->"if"
    Token.KW_ELSE->"else"
    Token.KW_FOR->"for"
    Token.KW_CONTINUE->"continue"
    Token.KW_BREAK->"break"
    Token.KW_RETURN->"return"
    Token.KW_SIZEOF->"sizeof"
    Token.KW_ALIGN->"__align"
    Token.KW_CDECL->"__cdecl"
    Token.KW_STDCALL->"__stdcall"
  }
  def readSourceFile(file:File):String = {
    Source.fromFile(file).getLines().reduce(_+_)
  }


}
