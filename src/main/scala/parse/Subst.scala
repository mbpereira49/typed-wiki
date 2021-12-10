import cats.parse.Rfc5234.{alpha, sp, char, wsp, lf, digit, cr, dquote, vchar}
import cats.parse.{Parser => P, Parser0 => P0}

val argument: P[(Expr.Identifier, Expr)] = identifier + (equals >> expr)
val argument_map : P0[Map[Expr.Identifier, Expr]] = list(argument).map(l => l.toMap)
val construction : P[Construction] = (identifier ~ (leftParen >> argument_map << rightParen)).map(x => 
    x match
        case (id: Expr.Identifier, args: Map[Expr.Identifier, Expr]) => Construction(id, args)
)

val object_declaration: P[ObjectDeclaration] = ((declaration << equals) + construction).map(
    x => x match
        case ((id: Expr.Identifier, t: Type), c: Construction) => ObjectDeclaration(id, t, c)
)

val subst: P0[Subst] = empty *> object_declaration.repSep0(lf ~ empty).map(Subst(_)) <* empty