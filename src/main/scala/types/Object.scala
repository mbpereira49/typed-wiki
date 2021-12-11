package types

import parse.ast.{Identifier, Expr}

trait Function:
    def run(env: Env)(objs: List[Object]): Object

class Method(val args: Map[Identifier, Type], val body: Expr) extends Function:
    def run(env: Env)(objs: List[Object]): Object =
        val bindings: Map[Identifier, Object] = (args.keys zip objs).toMap
        val new_env: Env = Env(env.mapping ++ bindings)
        eval.evalExpr(body, new_env)

class Env(var mapping: Map[Identifier, Object] = Map()):
    def update(new_mappings: Map[Identifier, Object]): Unit = 
        mapping = mapping ++ new_mappings

class Type(val id: Identifier, val data: Map[Identifier, Type], val methods: Map[Identifier, Function])

trait Object:
    val t: Type

trait Property:
    val properties: Map[Identifier, Object]

class Class(val t: Type, val properties: Map[Identifier, Object]) extends Object, Property