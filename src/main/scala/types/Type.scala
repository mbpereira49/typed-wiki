package types

import parse.ast.Identifier

sealed trait Type:
    val methods: Map[Identifier, Implementation]

class BaseType(val id: Identifier, val methods: Map[Identifier, types.Implementation]) extends Type
class ClassType(val id: Identifier, val data: Map[Identifier, types.Implementation], val methods: Map[Identifier, Implementation]) extends Type
class ListType(t: Type) extends Type:
    val data = Map()
    val methods = Map()
class MapType(from: Type, to: Type) extends Type:
    val data = Map()
    val methods = Map()
class TupleType(t1: Type, t2: Type) extends Type:
    val data = Map()
    val methods = Map()