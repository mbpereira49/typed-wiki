package types

import parse.ast.Identifier

trait Object:
    val t: Type

trait Property:
    val properties: Map[Identifier, Object]

class Class(val t: Type, val properties: Map[Identifier, Object]) extends Object, Property