import cats.parse.Rfc5234.{alpha, sp, vchar, wsp, lf}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.parse.Parser.not

val id: P[String] = alpha.rep.string
val typeId: P[Type] = P.recursive[Type] { recurse =>
    recurse.between(P.string("List["), P.string("]")).map(Type.ListType(_)) | id.map(Type.Identifier(_))
}
val method: P[String] = (alpha.rep.string).repSep(wsp).string

val any_sp0: P0[Unit] = (wsp | lf).rep0.void
val equals: P[Unit] = P.char('=').surroundedBy(any_sp0)

def with_wsp[A](p: P0[A]): P0[A] = p.surroundedBy(any_sp0)
def with_wsp[A](p: P[A]): P[A] = p.surroundedBy(any_sp0)

val declaration: P[(String, Type)] = (id ~ (P.char(':').surroundedBy(any_sp0) *> typeId)).surroundedBy(any_sp0)
//val implementation: P[(String, String)] = (id ~ (equals *> method)).surroundedBy(any_sp0)

def list[A] (p : P[A]): P0[List[A]] = with_wsp(p.repSep0(P.char(',').surroundedBy(any_sp0)))
def bracket_list[A] (p : P[A]): P[List[A]] = P.char('[') *> list(p) <* P.char(']')

val declaration_list: P[List[(String, Type)]] = bracket_list(declaration)

val data: P[Data] = (P.string("data") *> (equals *> declaration_list)).surroundedBy(any_sp0).map(l => Data(l.toMap))
val methods: P[Methods] = (P.string("methods") *> (equals *> declaration_list)).surroundedBy(any_sp0).map(l => Methods(l.toMap))

val field: P[Field] = data | methods
val fields: P[List[Field]] = bracket_list(field)

val extend: P[List[Relation]] = (with_wsp(P.string("extends")) *> list(with_wsp(id))).map(l => l.map(s => Extends(s)))
val implement: P[List[Relation]] = (with_wsp(P.string("implements")) *> list(with_wsp(id))).map(l => l.map(s => Implements(s)))
val class_definition  : P[ClassDef] = 
  (P.string("class") *> 
  with_wsp(id) ~ (extend.? ~ implement.?) ~ (equals *> fields))
  .map(x => x match 
    case ((id: String, (e: Option[List[Relation]], i: Option[List[Relation]])), fields: List[Field]) =>
      val i_list = i match {
          case Some(l) => l
          case None => Nil
      }
      val e_list = e match {
          case Some(l) => l
          case None => Nil
      }
      ClassDef(id, e_list ++ i_list, fields)
)
