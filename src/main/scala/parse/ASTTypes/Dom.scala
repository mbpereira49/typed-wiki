case class Dom(l: Seq[Type])
case class TypeDef(name: String, parent: Option[String], interfaces: Seq[String], fields: Seq[Field])

sealed trait Field
case class Data(m: Map[String, Type]) extends Field
case class Methods(m: Map[String, String]) extends Field

enum Type:
  case Identifier(s: String)
  case ListType(t: Type)
