package parse.ast

case class Identifier(s: String)

enum Lit:
  case Number(i: Int)
  case Str(s: String)

enum Attribute:
  case Property(name: Identifier)
  case Method(name: Identifier, args: List[Expr])

enum Expr:
  case Var(id: Identifier)
  case Literal(l: Lit)
  case Plus(e1: Expr, e2: Expr)
  case Access(e: Expr, a: Attribute)

enum Type:
  case Identifier(s: String)
  case ListType(t: Type)
  case MapType(from: Type, to: Type)
  case TupleType(ts: List[Type])