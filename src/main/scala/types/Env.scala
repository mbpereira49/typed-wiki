package types

import parse.ast.Identifier
import scala.collection.mutable

class Env(var mapping: mutable.Map[Identifier, types.Object] = mutable.Map()):
    def add(id: Identifier, o: types.Object): scala.Unit =
        mapping += (id -> o)

class TypeEnv(var mapping: mutable.Map[Identifier, Type] = mutable.Map()):
    def addType(t: IDType): scala.Unit = 
        mapping += (t.id -> t)
    override def toString = s"TypeEnv($mapping)"