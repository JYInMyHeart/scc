class Node
class ExprNode extends Node
class StmtNode extends Node
class TypeNode(name:String) extends Node
class ForNode(init:StmtNode,
              cond:ExprNode,
              incr:StmtNode,
              body:StmtNode) extends StmtNode
class IfNode(cond:ExprNode,
             thenBody:StmtNode,
             elseBody:StmtNode) extends StmtNode
class BreakNode extends StmtNode
class ContinueNode extends StmtNode
class ReturnNode extends StmtNode
class BlockNode extends StmtNode
class ExprStmtNode extends StmtNode
class BinaryOpNode(op:String,
                   left:ExprNode,
                   right:ExprNode,
                   t:TypeCode.Value) extends ExprNode
class StructNode(name:String,
                 t:TypeCode.Value,
                 member:List[Slot])

class LiteralNode(typeNode: TypeNode) extends ExprNode
class IntegerLiteralNode(value:Long,t:TypeNode) extends LiteralNode(t)
class StringLiteralNode(value:String,t:TypeNode) extends LiteralNode(t)
class SizeofExprNode(exprNode: ExprNode,
                     typeNode: TypeNode) extends ExprNode
class AssignNode(lhs:ExprNode,
                 rhs:ExprNode,
                 op:String) extends ExprNode
class FuncallNode(exprNode: ExprNode,
                  args:List[ExprNode]) extends ExprNode
class UnaryOpNode(op:String,
                  exprNode: ExprNode,
                  t:TypeCode.Value) extends ExprNode

class Slot(typeNode: TypeNode,
           name:String,
           offset:Long) extends Node














case class Ast(translationUnit: TranslationUnit)

case class TranslationUnit(externDeclaration: List[ExternDeclaration],
                      eof: Token.Value)  {
  override def toString: String = s"\nTranslationUnit<$externDeclaration $eof>".replace("null", "")
}


case class ExternDeclaration(typeSpecifier: TypeSpecifier,
                        functionDefinition: List[FunctionDefinition],
                        initDeclarator: List[InitDeclarator]) {
  override def toString: String = s"\nExternDeclaration:$typeSpecifier $functionDefinition $initDeclarator".replace("null", "")

}

case class InitDeclarator(declarator: Declarator,
                     assign: AssignExpr) {
  override def toString: String = s"\nInitDeclarator:$declarator $assign".replace("null", "")
}

case class FunctionDefinition(declarator: Declarator,
                         funcbody: Funcbody) {
  override def toString: String = s"\nFunctionDefinition:$declarator $funcbody".replace("null", "")
}

case class Declaration(typeSpecifier: TypeSpecifier,
                  initDeclaratorList: List[InitDeclaratorList]) {
  override def toString: String = s"\nDeclaration:$typeSpecifier $initDeclaratorList".replace("null", "")
}

case class InitDeclaratorList(initDeclarator: InitDeclarator,
                         initdecl: List[InitDeclarator]) {
  override def toString: String = s"\nInitDeclaratorList:$initDeclarator $initdecl".replace("null", "")
}


case class AssignExpr(assign: Token.Value,
                 initializer: Initializer) {
  override def toString: String = s"\nAssign:$assign $initializer".replace("null", "")
}

case class TypeSpecifier(t: Token.Value, structSpecifier: StructSpecifier) {
  override def toString: String = s"\nTypeSpecifier:$t $structSpecifier".replace("null", "")
}

case class StructSpecifier(kwStruct: Token.Value,
                      identifier: Token.Value,
                      structDeclaration: List[StructDeclaration]) {
  override def toString: String = s"\nStructSpecifier:$kwStruct $identifier $structDeclaration"
}

case class StructDeclaration(typeSpecifier: TypeSpecifier,
                        decl: List[Declarator]) {
  override def toString: String = s"\nStructDeclaration:$typeSpecifier $decl".replace("null", "")
}




case class FunctionCallingConvention(token: Token.Value) {
  override def toString: String = s"\nFunc:$token".replace("null", "")
}

case class StructMemberAlignment(align: Token.Value,
                            cint: Token.Value) {
  override def toString: String = s"\nStructMemberAlignment:$align $cint".replace("null", "")
}

case class Declarator(pointer: Token.Value,
                 functionCallingConvention: FunctionCallingConvention,
                 structMemberAlignment: StructMemberAlignment,
                 directDeclarator: DirectDeclarator) {
  override def toString: String = s"\nDeclarator:$pointer $functionCallingConvention $structMemberAlignment $directDeclarator".replace("null", "")
}

case class DirectDeclarator(identifier: Token.Value,
                       directDeclaratorPostfix: DirectDeclaratorPostfix) {
  override def toString: String = s"\nDirectDeclarator:$identifier $directDeclaratorPostfix".replace("null", "")
}

case class DirectDeclaratorPostfix(cint: Token.Value,
                              parameterTypeList: ParameterTypeList) {
  override def toString: String = s"\nDirectDeclaratorPostfix:$cint $parameterTypeList".replace("null", "")
}


case class ParameterTypeList(parameterList: ParameterList,
                        comma: Token.Value,
                        ellipsis: Token.Value) {
  override def toString: String = s"\nParameterTypeList:$parameterList $comma $ellipsis".replace("null", "")
}

case class ParameterList(parameterDeclaration: ParameterDeclaration,
                    parameterDecl: ParameterDeclaration*) {
  override def toString: String = s"\nParameterList:$parameterDeclaration".replace("null", "")
}


case class ParameterDeclaration(typeSpecifier: TypeSpecifier,
                           declarator: Declarator) {
  override def toString: String = s"$typeSpecifier $declarator".replace("null", "")
}

case class Funcbody(compoundStatement: CompoundStatement) {
  override def toString: String = s"\nFuncbody:$compoundStatement".replace("null", "")
}

case class Initializer(assignmentExpression: AssignmentExpression) {
  override def toString: String = s"\nInitializer:$assignmentExpression".replace("null", "")
}

case class CompoundStatement(internDeclaration: List[ExternDeclaration],
                        statement: List[Statement]) {
  override def toString: String = s"\nCompoundStatement:$internDeclaration $statement".replace("null", "")
}

case class Statement(compoundStatement: CompoundStatement,
                ifStatement: IfStatement,
                returnStatement: ReturnStatement,
                breakStatement: BreakStatement,
                continueStatement: ContinueStatement,
                forStatement: ForStatement,
                expressionStatement: ExpressionStatement) {
  override def toString: String = s"\nStatement:$compoundStatement $ifStatement $returnStatement $breakStatement $continueStatement $forStatement $expressionStatement".replace("null", "")
}

case class ExpressionStatement(expression: Expression,
                          semicolon: Token.Value) {
  override def toString: String = s"\nExpressionStatement:$expression $semicolon".replace("null", "")
}

case class IfStatement(iff: Token.Value,
                  expression: Expression,
                  statement: Statement,
                  elsee: Token.Value,
                  statements: Statement) {
  override def toString: String = s"\nIfStatement:$iff $expression $statement $elsee $statements".replace("null", "")
}

case class ForStatement(forr: Token.Value,
                   expressionStatement1: ExpressionStatement,
                   expressionStatement2: ExpressionStatement,
                   expression: Expression,
                   statement: Statement) {
  override def toString: String = s"\nForStmt:$forr $expressionStatement1 $expressionStatement2 $expression $statement".replace("null", "")
}

case class ContinueStatement(continue: Token.Value) {
  override def toString: String = s"\nContinueStmt:$continue".replace("null", "")
}

case class BreakStatement(break: Token.Value) {
  override def toString: String = s"\nBreakStmt:$break".replace("null", "")
}

case class ReturnStatement(returnn: Token.Value,
                      expression: Expression) {
  override def toString: String = s"\nReturnStmt:$returnn $expression".replace("null", "")
}

case class Expression(assignmentExpression: List[AssignmentExpression]) {
  override def toString: String = if (assignmentExpression.nonEmpty)
    s"\nExpr:$assignmentExpression".replace("null", "")
  else
    ""
}


case class AssignmentExpression(equalityExpression: EqualityExpression,
                           assign: Token.Value,
                           assignmentExpression: AssignmentExpression) {
  override def toString: String = s"\nAssignmentExpr:$equalityExpression $assign $assignmentExpression".replace("null", "")
}

case class EqualityExpression(relationalExpression: RelationExpression,
                         eq: Token.Value,
                         relationalExpression1: RelationExpression) {
  override def toString: String = s"\nEqualityExpr:$relationalExpression $eq $relationalExpression1".replace("null", "")
}

case class RelationExpression(additiveExpression: AdditiveExpression,
                         token: Token.Value,
                         additiveExpression1: AdditiveExpression) {
  override def toString: String = s"\nRelationExpr:$additiveExpression $token $additiveExpression1".replace("null", "")
}

case class AdditiveExpression(multplicativeExpression: MultplicativeExpression,
                         token: Token.Value,
                         multplicativeExpression1: MultplicativeExpression) {
  override def toString: String = s"\nAdditiveExpr:$multplicativeExpression $token $multplicativeExpression1".replace("null", "")
}

case class MultplicativeExpression(unaryExpression: UnaryExpression,
                              token: Token.Value,
                              unaryExpression1: UnaryExpression) {
  override def toString: String = s"\nMultplicativeExpr:$unaryExpression $token $unaryExpression".replace("null", "")
}

case class UnaryExpression(token: Token.Value,
                      unaryExpression: UnaryExpression,
                      postfixExpression: PostfixExpression,
                      sizeofExpression: SizeofExpression) {
  override def toString: String = s"\nUnaryExpr:$token $unaryExpression $postfixExpression $sizeofExpression".replace("null", "")
}

case class SizeofExpression(sizeof: Token.Value,
                       typeSpecifier: TypeSpecifier) {
  override def toString: String = s"\nSizeOfExpr:$sizeof $typeSpecifier".replace("null", "")
}

case class PostfixExpression(primaryExpression: PrimaryExpression,
                        expression: Expression,
                        arguementExpressionList: ArguementExpressionList,
                        dotExpression: DotExpression,
                        pointstoExpression: PointstoExpression) {
  override def toString: String = s"\nPostfixExpr:$primaryExpression $expression $arguementExpressionList $dotExpression $pointstoExpression".replace("null", "")
}

case class DotExpression(dot: Token.Value,
                    identifier: Token.Value) {
  override def toString: String = s"\nDotExpr:$dot $identifier".replace("null", "")
}

case class PointstoExpression(pointsto: Token.Value,
                         identifier: Token.Value) {
  override def toString: String = s"\nPointstoExpr:$pointsto $identifier".replace("null", "")
}

case class ArguementExpressionList(moreArguement: List[AssignmentExpression]) {
  override def toString: String = s"\nArguementExpr:$moreArguement".replace("null", "")
}


case class PrimaryExpression(token: Token.Value,
                        paExpression: Expression) {
  override def toString: String = s"\nPrimaryExpr:$token $paExpression".replace("null", "")
}



