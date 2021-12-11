package types

import scala.collection.mutable.Map
import parse.ast.{Identifier, Expr}

class Function(val args: Map[Identifier, Type], val body: Expr)

class Env(var mapping: Map[Identifier, Object]):
    def update(new_mappings: Map[Identifier, Object]): Unit = 
        mapping = mapping ++ new_mappings

class Type(val id: Identifier, val data: Map[Identifier, Type], val methods: Map[Identifier, Function])

trait Object:
    val t: Type

trait Property:
    val properties: Map[Identifier, Object]

class Class(val t: Type, val properties: Map[Identifier, Object]) extends Object, Property