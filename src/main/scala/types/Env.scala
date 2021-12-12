package types

import parse.ast.Identifier
import scala.collection.mutable

class Env(var mapping: mutable.Map[Identifier, Object] = mutable.Map())

class TypeEnv(var mapping: mutable.Map[Identifier, Type] = mutable.Map()):
    def addType(t: IDType): scala.Unit = 
        mapping += (t.id -> t)