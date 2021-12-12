package types

import parse.ast.Identifier
import types.Function
import types.Implementation.Implemented

sealed trait Value:
    val value: Any
    override def toString: Predef.String = value.toString
    

object Unit:
    val t = BaseType(Identifier("Unit"), Map())
class Unit extends Object, Value:
    val value: scala.Unit = ()
    val t = Unit.t

object String:
    val plus: NativeFunction = NativeFunction(self => objs =>
            scala.Console.flush()
            (self, objs) match {
                case (_, Nil) => throw Exception("'+' takes second argument")
                case (self: types.String, (that : types.String) :: tl) => types.String(self.value + that.value)
                case _ => throw Exception("Type of second argument to '+' does not match String")
            },
            MapType(t, t)
        )
    val methods= Map[Identifier, types.Implementation](Identifier("__plus") -> Implemented(MapType(t, t), plus))
    val t: BaseType = BaseType(Identifier("String"), methods)
class String(s: Predef.String) extends Object, Value:
    val value: Predef.String = s
    val t = String.t

object Int:
    val toStr = NativeFunction(self => l => 
        self match {
            case (self: Value) => types.String(self.value.toString)
            case _ => throw Exception("self not Value. Should not reach this but don't know how to guarantee")
        },
        MapType(Unit.t, String.t)
    )
    val plus: NativeFunction = NativeFunction(self => objs =>
        (self, objs) match {
            case (_, Nil) => throw Exception("'+' takes second argument")
            case (self: types.Int, (that : types.Int) :: tl) => types.Int(self.value + that.value)
            case _ => throw Exception("Type of second argument to '+' does not match Int")
        },
        MapType(t, t)
    )
    val methods = Map[Identifier, types.Implementation](
        Identifier("__plus") -> Implemented(MapType(t, t), plus),
        Identifier("str") -> Implemented(MapType(t, t), toStr)
    )

    val t: BaseType = BaseType(Identifier("Int"), methods)
class Int(i: scala.Int) extends Object, Value:
    val value: scala.Int = i
    val t = Int.t