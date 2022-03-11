package types

import parse.ast.{Identifier, Expr}

trait Object:
    val t: Type

trait Callable extends Object:
    def call(objs: List[Object], env: Env, self: Object): Object

trait Property:
    val properties: Map[Identifier, Object]

class ClassInstance(val t: Type, val properties: Map[Identifier, Object]) extends Object, Property