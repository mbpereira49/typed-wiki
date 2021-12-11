package types

class String(s: Predef.String) extends Object:
    val t = Type("String", List(), List())
    val state = State(List())
class Int(i: scala.Int) extends Object:
    val t = Type("Int", List(), List())
    val state = State(List())