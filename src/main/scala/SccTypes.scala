import scala.collection.mutable

class SccTypes
case class Symbol(var name:String,
                  var parentScope:Scope,
                  var t:TypeCode.Value,
                  var loc:Int,
                  var next:Symbol,
                  var currentScope:Scope){
}
case class Scope(var env:mutable.Stack[Symbol],
                 var scopes:List[Scope])

object TypeCode extends Enumeration{
  val T_INT = Value(0)
  val T_CHAR = Value(1)
  val T_SHORT = Value(2)
  val T_VOID = Value(3)
  val T_PTR = Value(4)
  val T_FUNC = Value(5)
  val T_STRUCT = Value(6)
  val T_BTYPE = Value(0x000f)
  val T_ARRAY = Value(0x0010)

  def |(t1:TypeCode.Value,t2:TypeCode.Value): TypeCode.Value = Value(t1.id | t2.id)
  def &(t1:TypeCode.Value,t2:TypeCode.Value): TypeCode.Value = Value(t1.id & t2.id)
}
