package types

import scala.collection.mutable.Map

class State(fields: List[Predef.String]):
    var values: Map[Predef.String, Object] = Map[Predef.String, Object]()

    def update_state(id: Predef.String, obj: Object): Unit =
        values(id) = obj

class Type(val id: Predef.String, val data: List[Predef.String], val methods: List[Predef.String])

trait Object:
    val state: State
    val t: Type
class Class(val t: Type) extends Object:
    val state = State(t.data)