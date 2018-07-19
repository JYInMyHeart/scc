case class Ast(translationUnit: TranslationUnit)

case class TranslationUnit(externDeclaration: ExternDeclaration,
                           eof: Token.Value)


case class ExternDeclaration(typeSpecifier: TypeSpecifier)
                            (semicolon: Token.Value*)
                            (functionDefinition: FunctionDefinition*)
                            (initDeclarator: InitDeclarator*)

case class InitDeclarator(comma: Token.Value*)
                         (declarator: Declarator,
                          assign: AssignExpr)

case class FunctionDefinition(declarator: Declarator,
                              funcbody: Funcbody)

case class Declaration(typeSpecifier: TypeSpecifier,
                       initDeclaratorList: List[InitDeclaratorList],
                       semicolon: Token.Value)

case class InitDeclaratorList(initDeclarator: InitDeclarator,
                              initdecl: List[InitDecl])

case class InitDecl(comma: Token.Value,
                    initDeclarator: InitDeclarator)


case class AssignExpr(assign: Token.Value,
                      initializer: Initializer)

case class TypeSpecifier(t: Token.Value*)(structSpecifier: StructSpecifier*)

case class StructSpecifier(kwStruct: Token.Value,
                           identifier: Token.Value,
                           structDecl: StructDecl*)

case class StructDecl(begin: Token.Value,
                      structDeclaration: StructDeclaration,
                      end: Token.Value)


case class StructDeclaration(typeSpecifier: TypeSpecifier,
                             decl: Decl,
                             semicolon: Token.Value)


case class Decl(comma: Token.Value,
                declarator: Declarator)

case class FunctionCallingConvention(token: Token.Value)

case class StructMemberAlignment(align: Token.Value,
                                 openpa: Token.Value,
                                 cint: Token.Value,
                                 closepa: Token.Value)

case class Declarator(pointer: Token.Value*)
                     (functionCallingConvention: FunctionCallingConvention*)
                     (structMemberAlignment: StructMemberAlignment*)
                     (directDeclarator: DirectDeclarator*)

case class DirectDeclarator(identifier: Token.Value,
                            directDeclaratorPostfix: DirectDeclaratorPostfix)

case class DirectDeclaratorPostfix(br: Br*)
                                  (pa: Pa*)

case class Br(openbr: Token.Value)
             (cint: Token.Value*)
             (closebr: Token.Value)

case class Pa(openpa: Token.Value)
             (parameterTypeList: ParameterTypeList*)
             (closepa: Token.Value)


case class ParameterTypeList(parameterList: ParameterList)
                            (comma: Token.Value*)
                            (ellipsis: Token.Value*)

case class ParameterList(parameterDeclaration: ParameterDeclaration)
                        (parameterDecl: ParameterDecl*)

case class ParameterDecl(comma: Token.Value,
                         parameterDeclaration: ParameterDeclaration)

case class ParameterDeclaration(typeSpecifier: TypeSpecifier)
                               (declarator: Declarator*)

case class Funcbody(compoundStatement: CompoundStatement)

case class Initializer(assignmentExpression: AssignmentExpression)

case class CompoundStatement(begin: Token.Value)
                            (declaration: Declaration*)
                            (statement: Statement*)
                            (end: Token.Value)

case class Statement(compoundStatement: CompoundStatement*)
                    (ifStatement: IfStatement*)
                    (returnStatement: ReturnStatement*)
                    (breakStatement: BreakStatement*)
                    (continueStatement: ContinueStatement*)
                    (forStatement: ForStatement)
                    (expressionStatement: ExpressionStatement*)

case class ExpressionStatement(expression: Expression*)
                              (semicolon: Token.Value)

case class IfStatement(iff: Token.Value,
                       openpa: Token.Value,
                       expression: Expression,
                       closepa: Token.Value,
                       statement: Statement)
                      (elsee: Token.Value*)
                      (statements: Statement*)

case class ForStatement(forr: Token.Value,
                        openpa: Token.Value,
                        expressionStatement1: ExpressionStatement,
                        expressionStatement2: ExpressionStatement,
                        expression: Expression,
                        closepa: Token.Value,
                        statement: Statement)

case class ContinueStatement(continue: Token.Value,
                             semicolon: Token.Value)

case class BreakStatement(break: Token.Value,
                          semicolon: Token.Value)

case class ReturnStatement(returnn: Token.Value)
                          (expression: Expression*)
                          (semicolon: Token.Value)

case class Expression(assignmentExpression: AssignmentExpression)
                     (moreExpr: MoreArguement*)


case class AssignmentExpression(equalityExpression: EqualityExpression)
                               (assign: Token.Value*)
                               (assignmentExpression: AssignmentExpression*)

case class EqualityExpression(relationalExpression: RelationExpression)
                             (eq: Token.Value*)
                             (relationalExpression1: RelationExpression*)

case class RelationExpression(additiveExpression: AdditiveExpression)
                             (token: Token.Value*)
                             (additiveExpression1: AdditiveExpression*)

case class AdditiveExpression(multplicativeExpression: MultplicativeExpression)
                             (token: Token.Value*)
                             (multplicativeExpression1: MultplicativeExpression*)

case class MultplicativeExpression(unaryExpression: UnaryExpression)
                                  (token: Token.Value*)
                                  (unaryExpression1: UnaryExpression*)

case class UnaryExpression(token: Token.Value*)
                          (unaryExpression: UnaryExpression*)
                          (postfixExpression: PostfixExpression*)
                          (sizeofExpression: SizeofExpression*)

case class SizeofExpression(sizeof: Token.Value,
                            openpa: Token.Value,
                            typeSpecifier: TypeSpecifier,
                            closepa: Token.Value)

case class PostfixExpression(primaryExpression: PrimaryExpression)
                            (brExpression: BrExpression*)
                            (paExpression: PaArgueExpression*)
                            (dotExpression: DotExpression*)
                            (pointstoExpression: PointstoExpression*)

case class BrExpression(openbr: Token.Value)
                       (expression: Expression)
                       (closebr: Token.Value)

case class PaExpression(openpa: Token.Value)
                       (expression: Expression)
                       (closepa: Token.Value)

case class PaArgueExpression(openpa: Token.Value)
                            (arguementExpressionList: ArguementExpressionList*)
                            (closepa: Token.Value)

case class DotExpression(dot: Token.Value,
                         identifier: Token.Value)

case class PointstoExpression(pointsto: Token.Value,
                              identifier: Token.Value)

case class ArguementExpressionList(assignmentExpression: AssignmentExpression)
                                  (moreArguement: MoreArguement*)

case class MoreArguement(comma: Token.Value,
                         assignmentExpression: AssignmentExpression)

case class PrimaryExpression(token: Token.Value)
                            (paExpression: PaExpression*)



