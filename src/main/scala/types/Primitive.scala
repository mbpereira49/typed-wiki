package types

import parse.ast.Identifier
import types.Function

sealed trait Value:
    val value: Any
    override def toString: Predef.String = value.toString
    

object Unit:
    val t = BaseType(Identifier("Unit"), Map(), Map())
class Unit extends Object, Value:
    val value: scala.Unit = ()
    val t = Unit.t

object String:
    val plus: NativeFunction = NativeFunction(self => objs =>
            (self, objs) match {
                case (_, Nil) => throw Exception("'+' takes second argument")
                case (self: types.String, (that : types.String) :: tl) => types.String(self.value + that.value)
                case _ => throw Exception("Type of second argument to '+' does not match String")
            },
            MapType(t, t)
        )
    val methods = Map[Identifier, Function](Identifier("__plus") -> plus)
    val t = BaseType(Identifier("String"), Map(), methods)
class String(s: Predef.String) extends Object, Value:
    val value: Predef.String = s
    val t = String.t

object Int:
    val toStr = NativeFunction(self => l => types.String("self.value.toString"), MapType(Unit.t, String.t))
    val plus: NativeFunction = NativeFunction(self => objs =>
        (self, objs) match {
            case (_, Nil) => throw Exception("'+' takes second argument")
            case (self: types.Int, (that : types.Int) :: tl) => types.Int(self.value + that.value)
            case _ => throw Exception("Type of second argument to '+' does not match Int")
        },
        MapType(t, t)
    )
    val methods = Map[Identifier, Function](Identifier("str") -> toStr, Identifier("__plus") -> plus)
    val t = BaseType(Identifier("Int"), Map(), methods)
class Int(i: scala.Int) extends Object, Value:
    val value: scala.Int = i
    val t = Int.t