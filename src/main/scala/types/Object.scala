package types

import parse.ast.{Identifier, Expr}

trait Object:
    val t: Type

trait Property:
    val properties: Map[Identifier, Object]

class ClassInstance(val t: Type, val properties: Map[Identifier, Object]) extends Object, Property

class LazyObject(e: Expr) extends Object:
    val t = null
    val f : Env => Object => Object = { env => self => 
        eval.evalExpr(e, env, self)
    }