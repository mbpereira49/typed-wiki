import cats.parse.Rfc5234.{alpha, sp, vchar, wsp, lf}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.parse.Parser.not

val id: P[String] = alpha.rep.string
val method: P[String] = (alpha.rep.string).repSep(wsp).string

val any_sp0: P0[Unit] = (wsp | lf).rep0.void
val equals: P[Unit] = P.char('=').surroundedBy(any_sp0)

val variable_declaration: P[(String, String)] = (id ~ (P.char(':').surroundedBy(any_sp0) *> id)).surroundedBy(any_sp0)
val method_definition: P[(String, String)] = (id ~ (equals *> method)).surroundedBy(any_sp0)

def list[A] (p : P[A]): P0[List[A]] = p.repSep0(P.char(',').surroundedBy(any_sp0))
def bracket_list[A] (p : P[A]): P[List[A]] = P.char('[') *> list(p) <* P.char(']')

val data_list = bracket_list(variable_declaration)
val method_list = bracket_list(method_definition)

val data: P[Data] = (P.string("data") *> (equals *> data_list)).surroundedBy(any_sp0).map(l => Data(l.toMap))
val methods: P[Methods] = (P.string("methods") *> (equals *> method_list)).surroundedBy(any_sp0).map(l => Methods(l.toMap))

val field: P[Field] = data | methods
val fields: P[List[Field]] = bracket_list(field)

val type_ = (P.string("type") *> (id.surroundedBy(any_sp0) ~ (equals *> fields))).map((id, fields) => Type(id, None, Nil, fields))
