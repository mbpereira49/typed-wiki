package types

import parse.ast.Identifier
import types.Function

sealed trait Value:
    val value: Any
    override def toString: Predef.String = value.toString

class NativeFunction(f: List[Object] => Object) extends Function:
    def run(env : Env)(objs: List[Object]): Object = f(objs)

class String(s: Predef.String) extends Object, Value:
    val plus = NativeFunction(objs =>
        objs match {
            case Nil => throw Exception("'+' takes second argument")
            case (that : types.String) :: tl => types.String(value + that.value)
            case _ => throw Exception("Type of second argument to '+' does not match String")
        }
    )
    val methods = Map(Identifier("__plus") -> plus)
    val t = Type(Identifier("String"), Map(), methods)
    val value: Predef.String = s
class Int(i: scala.Int) extends Object, Value:
    val toStr = NativeFunction(l => types.String(value.toString))
    val plus = NativeFunction(objs =>
        objs match {
            case Nil => throw Exception("'+' takes second argument")
            case (that : types.Int) :: tl => types.Int(value + that.value)
            case _ => throw Exception("Type of second argument to '+' does not match Int")
        }
    )
    val methods = Map[Identifier, Function](Identifier("str") -> toStr, Identifier("__plus") -> plus)
    val t = Type(Identifier("Int"), Map(), methods)
    val value: scala.Int = i