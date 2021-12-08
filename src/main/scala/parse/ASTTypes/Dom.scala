case class Domain(l: Seq[Definition])

sealed trait Relation
case class Extends(s: String) extends Relation
case class Implements(s: String) extends Relation

sealed trait Definition
case class ClassDef(name: String, relations: Seq[Relation], fields: Seq[Field]) extends Definition
case class InterfaceDef(name: String, relations: Seq[Relation], fields: Seq[Field]) extends Definition

sealed trait Field
case class Data(m: Map[String, Type]) extends Field
case class Methods(m: Map[String, Type]) extends Field
case class Implementation(i: Expr.Identifier, body: Expr) extends Field

enum Lit:
  case Number(i: Int)
  case Str(s: String)

enum Expr:
  case Identifier(s: String)
  case Literal(l: Lit)
  case Plus(e1: Expr, e2: Expr)
  case FuncCall(ob: Identifier, method: Identifier, arg: List[Expr])

enum Type:
  case Identifier(s: String)
  case ListType(t: Type)
  case MapType(from: Type, to: Type)
