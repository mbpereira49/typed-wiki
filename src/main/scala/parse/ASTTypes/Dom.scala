case class Dom(l: Seq[Definition])

sealed trait Relation
case class Extends(s: String) extends Relation
case class Implements(s: String) extends Relation

sealed trait Definition
case class ClassDef(name: String, relations: Seq[Relation], fields: Seq[Field]) extends Definition
case class InterfaceDef(name: String, relations: Seq[Relation], fields: Seq[Field]) extends Definition

sealed trait Field
case class Data(m: Map[String, Type]) extends Field
case class Methods(m: Map[String, Type]) extends Field

enum Type:
  case Identifier(s: String)
  case ListType(t: Type)
  case MapType(from: Type, to: Type)
