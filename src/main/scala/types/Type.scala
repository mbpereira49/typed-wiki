package types

import parse.ast.Identifier

sealed trait Type:
    val data: Map[Identifier, Type]
    val methods: Map[Identifier, Function]

class BaseType(val id: Identifier, val data: Map[Identifier, Type], val methods: Map[Identifier, Function]) extends Type
class ListType(types: List[Type]) extends Type:
    val data = Map()
    val methods = Map()
class MapType(from: Type, to: Type) extends Type:
    val data = Map()
    val methods = Map()
class TupleType(t1: Type, t2: Type) extends Type:
    val data = Map()
    val methods = Map()