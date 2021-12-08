import cats.parse.Rfc5234.{alpha, sp, char, wsp, lf, digit, cr}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.parse.Parser.not

val any_sp0 = (wsp | lf | cr).rep0.void
extension[A](p : P[A])
  def +[B](that: P0[B]) : P[(A, B)] = p ~ (any_sp0 *> that)
  def >>[B](that: P0[B]) : P[B] = p *> (any_sp0 *> that)
  def <<[B](that: P[B]) : P[A] = p <* (any_sp0 <* that)

extension[A](p : P0[A])
  def +[B](that: P0[B]) : P0[(A, B)] = p ~ (any_sp0 *> that)
  def >>[B](that: P0[B]) : P0[B] = p *> (any_sp0 *> that)
  def <<[B](that: P[B]) : P0[A] = p <* (any_sp0 <* that)

val id: P[String] = alpha.rep.string
val typeId: P[Type] = P.recursive[Type] { recurse =>
    recurse.between(P.string("List["), P.string("]")).map(Type.ListType(_)) | id.map(Type.Identifier(_))
}

val declaration: P[(String, Type)] = (id + (P.char(':') >> typeId))

def list[A] (p: P[A]): P0[List[A]] = p.repSep0(P.char(',').surroundedBy(any_sp0).backtrack)
def bracket_list[A] (p : P[A]): P[List[A]] = P.char('[') >> list(p) << P.char(']')

val declaration_list: P[List[(String, Type)]] = bracket_list(declaration)

val data: P[Data] = (P.string("data") >> (P.char('=') >> declaration_list)).map(l => Data(l.toMap))
val methods: P[Methods] = (P.string("methods") >> (P.char('=') >> declaration_list)).map(l => Methods(l.toMap))

val number: P[Lit] = digit.rep.string.map(s => Lit.Number(s.toInt))
val valid_char: P[String] = alpha.string | P.charIn(" !@#$%^&*()-_=+[]{};:<>,./?").string | P.string("\\\"").string.as("\"")
val string: P[Lit] = (P.char('"') *> valid_char.repUntil0(P.char('"')).map(_.mkString) <* P.char('"')).map(s => Lit.Str(s))

val expr: P[Expr] = P.recursive[Expr] { recurse =>
  val identifier: P[Expr.Identifier] = id.map(x => Expr.Identifier(x))
  val literal: P[Expr.Literal] = (number | string).map(Expr.Literal(_))
  val func_call: P[Expr.FuncCall] = ((identifier <* P.char('.')) ~ identifier ~ (P.char('(') >> list(recurse) << P.char(')')))
    .map(x => x match 
      case ((id1: Expr.Identifier, id2: Expr.Identifier), e: List[Expr]) => Expr.FuncCall(id1, id2, e))
  val plus: P[Expr.Plus] = (((identifier.backtrack | literal.backtrack | func_call.backtrack | P.char('(') >> recurse << P.char(')')) << P.char('+')) + recurse).map(x => 
    x match 
      case (e1: Expr, e2: Expr) => Expr.Plus(e1, e2))
  plus.backtrack | func_call.backtrack | literal.backtrack | identifier
}

val let: P[Field] = ((P.string("let") >> id) + (P.char('=') >> expr))
  .map(x => x match
    case (id: String, e: Expr) => Implementation(Expr.Identifier(id), e))

val field: P[Field] = data | methods | let
val fields: P[List[Field]] = bracket_list(field)

val extend: P[List[Relation]] = (P.string("extends") >> list(id)).map(l => l.map(s => Extends(s)))
val implement: P[List[Relation]] = (P.string("implements") >> list(id)).map(l => l.map(s => Implements(s)))

def option_to_list[A](l : Option[List[A]]): List[A] =
  l match {
    case Some(l) => l
    case None => Nil
  }

val extend0: P0[List[Relation]] = extend.?.map(option_to_list)
val implement0: P0[List[Relation]] = implement.?.map(option_to_list)

val class_definition : P[ClassDef] = 
  (P.string("class") >> (id + (extend0 + implement0) + (P.char('=') >> fields)))
  .map(x => x match 
    case ((id: String, (e: List[Relation], i: List[Relation])), fields: List[Field]) =>
      ClassDef(id, e ++ i, fields)
  )

val interface_definition : P[InterfaceDef] =   
  (P.string("interface") >> (id + extend0 + (P.char('=') >> fields)))
  .map(x => x match 
    case ((id: String, i: List[Relation]), fields: List[Field]) =>
      InterfaceDef(id, i, fields)
  )

val definition = class_definition | interface_definition

val domain: P0[Domain] = definition.repSep0(lf.rep).map(Domain(_))