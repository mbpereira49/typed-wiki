package types

import parse.ast.Identifier

sealed trait Type:
    val methods: Map[Identifier, Implementation]
sealed trait IDType extends Type:
    val id: Identifier

class BaseType(val id: Identifier, val methods: Map[Identifier, types.Implementation]) extends IDType
class ClassType(val id: Identifier, val data: Map[Identifier, types.Implementation], val methods: Map[Identifier, Implementation]) extends IDType
class InterfaceType(val id: Identifier, val data: Map[Identifier, types.Implementation], val methods: Map[Identifier, Implementation]) extends IDType

class ListType(t: Type) extends Type:
    val data = Map()
    val methods = Map()
class MapType(from: Type, to: Type) extends Type:
    val data = Map()
    val methods = Map()
class TupleType(ts: List[Type]) extends Type:
    val data = Map()
    val methods = Map()