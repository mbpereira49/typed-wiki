import cats.parse.Rfc5234.{alpha, sp, char, wsp, lf, digit, cr, dquote, vchar}
import cats.parse.{Parser => P, Parser0 => P0}

import parse.ast.*

val id_char: P[Char] = alpha | P.charIn("_")
val id: P[String] = id_char.rep.string
val typeId: P[Type] = P.recursive[Type] { recurse =>
    recurse.between(P.string("List["), P.string("]")).map(Type.ListType(_)) | id.map(Type.Identifier(_))
}
val identifier: P[Expr.Identifier] = id.map(Expr.Identifier(_))

val number: P[Lit] = digit.rep.string.map(s => Lit.Number(s.toInt))
val valid_char: P[String] = alpha.string | digit.string | P.charIn(" !@#$%^&*()-_=+[]{};:<>,./?").string | P.string("\\\"").string.as("\"")
val string: P[Lit] = (dquote *> valid_char.repUntil0(dquote).map(_.mkString) <* dquote).map(s => Lit.Str(s))

val comment: P[Unit] = (P.string("//") ~ (vchar | wsp).rep0 ~ lf).void
val empty: P0[Unit] = (comment | any_sp).rep0.void