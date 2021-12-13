package types

import parse.ast.Identifier

sealed trait Type:
    val methods: Map[Identifier, Implementation]
sealed trait IDType extends Type:
    val id: Identifier
sealed trait StateType extends Type:
    val data: Map[Identifier, Implementation]

class BaseType(val id: Identifier, val methods: Map[Identifier, types.Implementation]) extends IDType
class ClassType(
    val id: Identifier, 
    val data: Map[Identifier, types.Implementation], 
    val methods: Map[Identifier, Implementation],
    val parents: Seq[ClassType],
    val interfaces: Seq[InterfaceType]) extends IDType, StateType
class InterfaceType(
    val id: Identifier, 
    val data: Map[Identifier, types.Implementation], 
    val methods: Map[Identifier, Implementation],
    val parents: Seq[InterfaceType]) extends IDType, StateType

class ListType(t: Type) extends Type:
    val methods = Map()
class MapType(from: Type, to: Type) extends Type:
    val methods = Map()
class TupleType(ts: List[Type]) extends Type:
    val methods = Map()