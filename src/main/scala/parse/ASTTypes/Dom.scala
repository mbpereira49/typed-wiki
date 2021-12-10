case class Domain(l: Seq[Definition])

sealed trait Relation
case class Extends(s: Expr.Identifier) extends Relation
case class Implements(s: Expr.Identifier) extends Relation

sealed trait Definition
case class ClassDef(name: Expr.Identifier, relations: Seq[Relation], fields: Seq[Field]) extends Definition
case class InterfaceDef(name: Expr.Identifier, relations: Seq[Relation], fields: Seq[Field]) extends Definition

sealed trait Field
case class Data(m: Map[Expr.Identifier, Type]) extends Field
case class Methods(m: Map[Expr.Identifier, Type]) extends Field
case class Implementation(i: Expr.Identifier, body: Expr) extends Field
