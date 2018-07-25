case class Ast(translationUnit: TranslationUnit)

case class TranslationUnit(externDeclaration: ExternDeclaration,
                           eof: Token.Value) {
  override def toString: String = s"TranslationUnit<$externDeclaration,$eof>"
}


case class ExternDeclaration(typeSpecifier: TypeSpecifier,
                             semicolon: Token.Value,
                             functionDefinition: List[FunctionDefinition],
                             initDeclarator: List[InitDeclarator]) {
  override def toString: String = s"\n\tExternDeclaration:$typeSpecifier,$semicolon,$functionDefinition,$initDeclarator"

}

case class InitDeclarator(comma: Token.Value,
                          declarator: Declarator,
                          assign: AssignExpr) {
  override def toString: String = s"\n\t\tInitDeclarator:$comma,$declarator,$assign"
}

case class FunctionDefinition(declarator: Declarator,
                              funcbody: Funcbody) {
  override def toString: String = s"\n\t\tFunctionDefinition:$declarator,$funcbody"
}

case class Declaration(typeSpecifier: TypeSpecifier,
                       initDeclaratorList: List[InitDeclaratorList],
                       semicolon: Token.Value) {
  override def toString: String = s"\n\t\tDeclaration:$typeSpecifier,$initDeclaratorList,$semicolon"
}

case class InitDeclaratorList(initDeclarator: InitDeclarator,
                              initdecl: List[InitDecl]) {
  override def toString: String = s"\n\t\tInitDeclaratorList:$initDeclarator,$initdecl"
}

case class InitDecl(comma: Token.Value,
                    initDeclarator: InitDeclarator) {
  override def toString: String = s"\n\t\tInitDecl:$comma,$initDeclarator"
}


case class AssignExpr(assign: Token.Value,
                      initializer: Initializer) {
  override def toString: String = s"\n\t\t\tAssign:$assign,$initializer"
}

case class TypeSpecifier(t: Token.Value, structSpecifier: StructSpecifier) {
  override def toString: String = s"\n\t\tTypeSpecifier:$t,$structSpecifier"
}

case class StructSpecifier(kwStruct: Token.Value,
                           identifier: Token.Value,
                           structDecl: StructDecl*) {
  override def toString: String = s"\n\t\tStructSpecifier:$kwStruct,$identifier,$structDecl"
}

case class StructDecl(begin: Token.Value,
                      structDeclaration: List[StructDeclaration],
                      end: Token.Value) {
  override def toString: String = s"\n\t\tStructDecl:$begin,$structDeclaration,$end"
}


case class StructDeclaration(typeSpecifier: TypeSpecifier,
                             decl: List[Decl],
                             semicolon: Token.Value) {
  override def toString: String = s"\n\t\tStructDeclaration:$typeSpecifier,$decl,$semicolon"
}


case class Decl(comma: Token.Value,
                declarator: Declarator) {
  override def toString: String = s"\n\t\tDecl:$comma,$declarator"
}

case class FunctionCallingConvention(token: Token.Value) {
  override def toString: String = s"\n\t\t\t\tFunc:$token"
}

case class StructMemberAlignment(align: Token.Value,
                                 openpa: Token.Value,
                                 cint: Token.Value,
                                 closepa: Token.Value) {
  override def toString: String = s"\n\t\t\t\tStructMemberAlignment:$align,$openpa,$cint,$closepa"
}

case class Declarator(pointer: Token.Value,
                      functionCallingConvention: FunctionCallingConvention,
                      structMemberAlignment: StructMemberAlignment,
                      directDeclarator: DirectDeclarator) {
  override def toString: String = s"\n\t\t\tDeclarator:$pointer,$functionCallingConvention,$structMemberAlignment,$directDeclarator"
}

case class DirectDeclarator(identifier: Token.Value,
                            directDeclaratorPostfix: DirectDeclaratorPostfix) {
  override def toString: String = s"\n\t\t\t\tDirectDeclarator:$identifier,$directDeclaratorPostfix"
}

case class DirectDeclaratorPostfix(br: Br,
                                   pa: Pa) {
  override def toString: String = s"\n\t\t\t\t\tDirectDeclaratorPostfix:$br,$pa"
}

case class Br(openbr: Token.Value,
              cint: Token.Value,
              closebr: Token.Value) {
  override def toString: String = s"$openbr,$cint,$closebr"
}

case class Pa(openpa: Token.Value,
              parameterTypeList: ParameterTypeList,
              closepa: Token.Value) {
  override def toString: String = s"$openpa,$parameterTypeList,$closepa"
}


case class ParameterTypeList(parameterList: ParameterList,
                             comma: Token.Value,
                             ellipsis: Token.Value) {
  override def toString: String = s"\n\t\tParameterTypeList:$parameterList,$comma,$ellipsis"
}

case class ParameterList(parameterDeclaration: ParameterDeclaration,
                         parameterDecl: ParameterDecl*) {
  override def toString: String = s"\n\t\tParameterList:$parameterDeclaration,$ParameterDeclaration"
}

case class ParameterDecl(comma: Token.Value,
                         parameterDeclaration: ParameterDeclaration) {
  override def toString: String = s"$comma,$parameterDeclaration"
}

case class ParameterDeclaration(typeSpecifier: TypeSpecifier,
                                declarator: Declarator) {
  override def toString: String = s"$typeSpecifier,$declarator"
}

case class Funcbody(compoundStatement: CompoundStatement) {
  override def toString: String = s"\n\t\tFuncbody:$compoundStatement"
}

case class Initializer(assignmentExpression: AssignmentExpression) {
  override def toString: String = s"\n\t\t\t\tInitializer:$assignmentExpression"
}

case class CompoundStatement(begin: Token.Value,
                             internDeclaration: List[ExternDeclaration],
                             statement: List[Statement],
                             end: Token.Value) {
  override def toString: String = s"\n\t\tCompoundStatement:$begin,$internDeclaration,$statement,$end"
}

case class Statement(compoundStatement: CompoundStatement,
                     ifStatement: IfStatement,
                     returnStatement: ReturnStatement,
                     breakStatement: BreakStatement,
                     continueStatement: ContinueStatement,
                     forStatement: ForStatement,
                     expressionStatement: ExpressionStatement) {
  override def toString: String = s"\n\t\tStatement:$compoundStatement,$ifStatement,$returnStatement,$breakStatement,$continueStatement,$forStatement,$expressionStatement"
}

case class ExpressionStatement(expression: Expression,
                               semicolon: Token.Value) {
  override def toString: String = s"\n\t\tExpressionStatement:$expression,$semicolon"
}

case class IfStatement(iff: Token.Value,
                       openpa: Token.Value,
                       expression: Expression,
                       closepa: Token.Value,
                       statement: Statement,
                       elsee: Token.Value,
                       statements: Statement) {
  override def toString: String = s"\n\t\tIfStatement:$iff,$openpa,$expression,$closepa,$statement,$elsee,$statements"
}

case class ForStatement(forr: Token.Value,
                        openpa: Token.Value,
                        expressionStatement1: ExpressionStatement,
                        expressionStatement2: ExpressionStatement,
                        expression: Expression,
                        closepa: Token.Value,
                        statement: Statement) {
  override def toString: String = s"\n\t\tForStmt:$forr,$openpa,$expressionStatement1,$expressionStatement2,$expression,$closepa,$statement"
}

case class ContinueStatement(continue: Token.Value,
                             semicolon: Token.Value) {
  override def toString: String = s"\n\t\tContinueStmt:$continue,$semicolon"
}

case class BreakStatement(break: Token.Value,
                          semicolon: Token.Value) {
  override def toString: String = s"\n\t\tBreakStmt:$break,$semicolon"
}

case class ReturnStatement(returnn: Token.Value,
                           expression: Expression,
                           semicolon: Token.Value) {
  override def toString: String = s"\n\t\tReturnStmt:$returnn,$expression,$semicolon"
}

case class Expression(assignmentExpression: AssignmentExpression,
                      moreExpr: MoreArguement*) {
  override def toString: String = s"\n\t\tExpr:$assignmentExpression,$moreExpr"
}


case class AssignmentExpression(equalityExpression: EqualityExpression,
                                assign: Token.Value,
                                assignmentExpression: AssignmentExpression) {
  override def toString: String = s"\n\t\t\t\t\tAssignmentExpr:$equalityExpression,$assign,$assignmentExpression"
}

case class EqualityExpression(relationalExpression: RelationExpression,
                              eq: Token.Value,
                              relationalExpression1: RelationExpression) {
  override def toString: String = s"\n\t\t\t\t\t\t\t\tEqualityExpr:$relationalExpression,$eq,$relationalExpression1"
}

case class RelationExpression(additiveExpression: AdditiveExpression,
                              token: Token.Value,
                              additiveExpression1: AdditiveExpression) {
  override def toString: String = s"\n\t\t\t\t\t\t\tRelationExpr:$additiveExpression,$token,$additiveExpression1"
}

case class AdditiveExpression(multplicativeExpression: MultplicativeExpression,
                              token: Token.Value,
                              multplicativeExpression1: MultplicativeExpression) {
  override def toString: String = s"\n\t\t\t\t\t\t\t\tAdditiveExpr:$multplicativeExpression,$token,$multplicativeExpression1"
}

case class MultplicativeExpression(unaryExpression: UnaryExpression,
                                   token: Token.Value,
                                   unaryExpression1: UnaryExpression) {
  override def toString: String = s"\n\t\t\t\t\t\t\t\t\tMultplicativeExpr:$unaryExpression,$token,$unaryExpression"
}

case class UnaryExpression(token: Token.Value,
                           unaryExpression: UnaryExpression,
                           postfixExpression: PostfixExpression,
                           sizeofExpression: SizeofExpression) {
  override def toString: String = s"\n\t\t\t\t\t\t\t\t\t\tUnaryExpr:$token,$unaryExpression,$postfixExpression,$sizeofExpression"
}

case class SizeofExpression(sizeof: Token.Value,
                            openpa: Token.Value,
                            typeSpecifier: TypeSpecifier,
                            closepa: Token.Value) {
  override def toString: String = s"\n\t\tSizeOfExpr:$sizeof,$openpa,$typeSpecifier,$closepa"
}

case class PostfixExpression(primaryExpression: PrimaryExpression,
                             brExpression: BrExpression,
                             paExpression: PaArgueExpression,
                             dotExpression: DotExpression,
                             pointstoExpression: PointstoExpression) {
  override def toString: String = s"\n\t\t\t\t\t\t\t\t\t\t\t\t\tPostfixExpr:$primaryExpression,$brExpression,$paExpression,$dotExpression,$pointstoExpression"
}

case class BrExpression(openbr: Token.Value,
                        expression: Expression,
                        closebr: Token.Value) {
  override def toString: String = s"$openbr,$expression,$closebr"
}

case class PaExpression(openpa: Token.Value,
                        expression: Expression,
                        closepa: Token.Value) {
  override def toString: String = s"$openpa,$expression,$closepa"
}

case class PaArgueExpression(openpa: Token.Value,
                             arguementExpressionList: ArguementExpressionList,
                             closepa: Token.Value) {
  override def toString: String = s"$openpa,$arguementExpressionList,$closepa"
}

case class DotExpression(dot: Token.Value,
                         identifier: Token.Value) {
  override def toString: String = s"\n\t\tDotExpr:$dot,$identifier"
}

case class PointstoExpression(pointsto: Token.Value,
                              identifier: Token.Value) {
  override def toString: String = s"\n\t\tPointstoExpr:$pointsto,$identifier"
}

case class ArguementExpressionList(assignmentExpression: AssignmentExpression,
                                   moreArguement: List[MoreArguement]) {
  override def toString: String = s"\n\t\tArguementExpr:$assignmentExpression,$moreArguement"
}

case class MoreArguement(comma: Token.Value,
                         assignmentExpression: AssignmentExpression) {
  override def toString: String = s"$comma,$assignmentExpression"
}

case class PrimaryExpression(token: Token.Value,
                             paExpression: PaExpression) {
  override def toString: String = s"\n\t\t\t\t\t\t\t\t\t\t\t\tPrimaryExpr:$token,$paExpression"
}



