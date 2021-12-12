package parse.ast

case class Domain(l: Seq[Definition])

sealed trait Relation
case class Extends(s: Identifier) extends Relation
case class Implements(s: Identifier) extends Relation

sealed trait Definition
case class ClassDef(name: Identifier, relations: Seq[Relation], fields: Seq[Field]) extends Definition
case class InterfaceDef(name: Identifier, relations: Seq[Relation], fields: Seq[Field]) extends Definition

sealed trait Field
case class Data(m: Map[Identifier, Type]) extends Field
case class Methods(m: Map[Identifier, Type]) extends Field
case class Assignment(i: Identifier, body: Expr) extends Field
case class Implementation(i: Identifier, args: Map[Identifier, Type], body: Expr) extends Field
