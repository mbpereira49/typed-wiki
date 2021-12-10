enum Lit:
  case Number(i: Int)
  case Str(s: String)

enum Attribute:
  case Property(name: Expr.Identifier)
  case Method(name: Expr.Identifier, args: List[Expr])

enum Expr:
  case Identifier(s: String)
  case Literal(l: Lit)
  case Plus(e1: Expr, e2: Expr)
  case Access(e: Expr, a: Attribute)

enum Type:
  case Identifier(s: String)
  case ListType(t: Type)
  case MapType(from: Type, to: Type) // not yet implemented