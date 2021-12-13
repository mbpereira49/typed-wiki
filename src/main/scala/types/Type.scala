package types

import parse.ast.Identifier

sealed trait Type:
    val methods: Map[Identifier, Implementation]
sealed trait IDType extends Type:
    val id: Identifier
    override def toString = id.toString
sealed trait StateType extends Type:
    val data: Map[Identifier, Implementation]

class BaseType(val id: Identifier, val methods: Map[Identifier, types.Implementation]) extends IDType
class ClassType(
    val id: Identifier, 
    val data: Map[Identifier, Implementation], 
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
    override def toString = s"MapType($from, $to)"
    val methods = Map()
class TupleType(ts: List[Type]) extends Type:
    val methods = Map()

def getType(t: parse.ast.Type, tenv: TypeEnv): types.Type =
    t match {
        case parse.ast.Type.Identifier(s) => 
            val id = Identifier(s)
            if tenv.mapping contains id then tenv.mapping(id) 
            else throw Exception(s"Type $id not defined")
        case parse.ast.Type.ListType(t) =>
            val c = getType(t, tenv)
            types.ListType(c)
        case parse.ast.Type.MapType(t1, t2) =>
            val c1 = getType(t1, tenv)
            val c2 = getType(t2, tenv)
            types.MapType(c1, c2)
        case parse.ast.Type.TupleType(ts) =>
            ts match {
                case Nil => types.Unit.t // If list is empty then Unit type
                case hd :: Nil => getType(hd, tenv) // If list has one element then return that type
                case ts => // else return TupleType
                    val cs = ts.map(t => getType(t, tenv)) 
                    types.TupleType(cs)
            }
    }