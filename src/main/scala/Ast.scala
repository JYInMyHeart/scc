
class Node

class ExprNode extends Node

class StmtNode extends Node

class TypeNode(val name: String) extends Node

class ForNode(val init: StmtNode,
              val cond: ExprNode,
              val incr: StmtNode,
              val body: StmtNode) extends StmtNode

class IfNode(val cond: ExprNode,
             val thenBody: StmtNode,
             val elseBody: StmtNode) extends StmtNode

class BreakNode extends StmtNode

class ContinueNode extends StmtNode

class ReturnNode extends StmtNode

class BlockNode(var varaiables: List[DefineVaribale],
                var stmts: List[StmtNode],
                var scope: LocalScope) extends StmtNode

class ExprStmtNode extends StmtNode

class BinaryOpNode(val op: String,
                   val left: ExprNode,
                   val right: ExprNode,
                   val t: TypeCode.Value) extends ExprNode

class StructNode(val name1: String,
                 val typeNode1: TypeNode,
                 var member: List[Slot]) extends TypeDefinition(name1, typeNode1)

class LiteralNode(val typeNode: TypeNode) extends ExprNode

class IntegerLiteralNode(val value: Long,val t: TypeNode) extends LiteralNode(t)

class StringLiteralNode(val value: String, val t: TypeNode) extends LiteralNode(t)

class SizeofExprNode(val exprNode: ExprNode,
                     val typeNode: TypeNode) extends ExprNode

class AssignNode(val lhs: ExprNode,
                 val rhs: ExprNode,
                 val op: String) extends ExprNode

class FuncallNode(val exprNode: ExprNode,
                  val args: List[ExprNode]) extends ExprNode

class UnaryOpNode(val op: String,
                  val exprNode: ExprNode,
                  val t: TypeCode.Value) extends ExprNode

class Slot(val typeNode: TypeNode,
           val name: String,
           val offset: Long) extends Node

class PtrMemberNode() extends ExprNode


class Scope(children: List[LocalScope])

class LocalScope(val parent: Scope,
                 var varaiables: Map[String, DefineVaribale],
                 var children: List[LocalScope]) extends Scope(children)

class TopScope(var staticLocalVariables: List[DefineVaribale],
               var entities: Map[String, Entity],
               var children: List[LocalScope]) extends Scope(children)

class TypeDefinition(val name: String,
                     val typeNode: TypeNode) extends Node

class TypedefNode(val real: TypeNode,
                  val name1: String,
                  val typeNode1: TypeNode) extends TypeDefinition(name1, typeNode1)

class MyAST(var declarations: Declarations,
            var scope: TopScope) extends Node

class Declarations(var defvars: Set[DefineVaribale],
                   var vardecls: Set[UndefinedVariable],
                   var defuns: Set[DefinedFunction],
                   var funcdecls: Set[UndefinedFunction],
                   var constants: Set[Constant],
                   var defstructs: Set[StructNode],
                   var typedefs: Set[TypedefNode]){
  def addVaraiables(v:DefineVaribale) = defvars += v
  def addFunctions(f:DefinedFunction) = defuns += f
}


class Entity(val name: String,
             val isPrivate: Boolean,
             val typeNode: TypeNode)

class Constant(val name1: String,
               val isPrivate1: Boolean,
               val typeNode1: TypeNode,
               val value: ExprNode) extends Entity(name1, isPrivate1, typeNode1)

class Function(override val name: String,
               override val isPrivate: Boolean,
               override val typeNode: TypeNode) extends Entity(name, isPrivate, typeNode)

class UndefinedFunction(override val name: String,
                        override val isPrivate: Boolean,
                        override val typeNode: TypeNode,
                         val params: Params) extends Function(name, isPrivate, typeNode)

class DefinedFunction(override val name: String,
                      override val isPrivate: Boolean,
                      override val typeNode: TypeNode,
                      val params: Params,
                      val body: BlockNode) extends Function(name, isPrivate, typeNode)

class DefineVaribale(val init: ExprNode,
                     val sequence: Long,
                     override val name: String,
                     override val isPrivate: Boolean,
                     override val typeNode: TypeNode) extends Variable(name, isPrivate, typeNode)

class UndefinedVariable(override val name: String,
                        override val isPrivate: Boolean,
                        override val typeNode: TypeNode) extends Variable(name, isPrivate, typeNode)

class Variable(override val name: String,
               override val isPrivate: Boolean,
               override val typeNode: TypeNode) extends Entity(name, isPrivate, typeNode)


class Parameter(override val init: ExprNode,
                override val sequence: Long,
                override val name: String,
                override val isPrivate: Boolean,
                override val typeNode: TypeNode) extends DefineVaribale(init, sequence, name, isPrivate, typeNode)

class ParamSlots[A](paramDescriptors: List[A],
                    val vararg: Boolean)

class Params(var paramDescriptors: List[Parameter],
             override val vararg: Boolean) extends ParamSlots[Parameter](paramDescriptors, vararg)

//class HomogeneousAst(token: Token.Value,
//                     var children:List[HomogeneousAst]){
//  def addChild(homogeneousAst: HomogeneousAst) = children +:= homogeneousAst
//  def isNil = token == null
//
//  def toStringTree(c:List[HomogeneousAst]): String = {
//    c match {
//      case Nil => this.toString
//      case x::xs =>
//        x.toString + toStringTree(xs)
//    }
//  }
//}

















