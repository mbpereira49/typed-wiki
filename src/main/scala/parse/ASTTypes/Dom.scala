case class Dom(l: Seq[Type])
case class Type(name: String, parent: Option[String], interfaces: Seq[String], fields: Seq[Field])

sealed trait Field
case class Data(m: Map[String, String]) extends Field
case class Methods(m: Map[String, String]) extends Field