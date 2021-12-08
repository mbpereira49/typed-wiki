import cats.parse.Rfc5234.{alpha, sp, vchar, wsp, lf}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.parse.Parser.not

val any_sp0 = (wsp | lf).rep0.void
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

val equals: P[Unit] = P.char('=').surroundedBy(any_sp0)

def with_wsp[A](p: P0[A]): P0[A] = p.surroundedBy(any_sp0)
def with_wsp[A](p: P[A]): P[A] = p.surroundedBy(any_sp0)

val declaration: P[(String, Type)] = (id + (P.char(':') >> typeId))

def list[A] (p: P[A]): P0[List[A]] = p.repSep0(P.char(',').surroundedBy(any_sp0).backtrack)
def bracket_list[A] (p : P[A]): P[List[A]] = P.char('[') *> list(p) <* P.char(']')

val declaration_list: P[List[(String, Type)]] = bracket_list(declaration)

val data: P[Data] = (P.string("data") >> (P.char('=') >> declaration_list)).map(l => Data(l.toMap))
val methods: P[Methods] = (P.string("methods") >> (P.char('=') >> declaration_list)).map(l => Methods(l.toMap))

val field: P[Field] = data | methods
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
val class_definition  : P[ClassDef] = 
  (P.string("class") >> (id + (extend0 + implement0) + (P.char('=') >> fields)))
  .map(x => x match 
    case ((id: String, (e: List[Relation], i: List[Relation])), fields: List[Field]) =>
      ClassDef(id, e ++ i, fields)
)

