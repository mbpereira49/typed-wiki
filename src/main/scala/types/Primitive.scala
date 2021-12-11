package types

import scala.collection.mutable.Map
import parse.ast.Identifier
import types.Function

sealed trait Value:
    val value: Any

class String(s: Predef.String) extends Object, Value:
    //val toString = Function(Map(), )
    val t = Type(Identifier("String"), Map(), Map())
    val value: Predef.String = s
class Int(i: scala.Int) extends Object, Value:
    val t = Type(Identifier("Int"), Map(), Map())
    val value: scala.Int = i