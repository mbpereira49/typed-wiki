import cats.parse.Rfc5234.{alpha, sp, char, wsp, lf, digit, cr, dquote, vchar}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.data.NonEmptyList

val id_char: P[Char] = alpha | P.charIn("_")
val id: P[String] = id_char.rep.string
val typeId: P[Type] = P.recursive[Type] { recurse =>
    recurse.between(P.string("List["), P.string("]")).map(Type.ListType(_)) | id.map(Type.Identifier(_))
}
val identifier: P[Expr.Identifier] = id.map(Expr.Identifier(_))