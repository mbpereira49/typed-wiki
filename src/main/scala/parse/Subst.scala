package parse

import cats.parse.Rfc5234.{alpha, sp, char, wsp, lf, digit, cr, dquote, vchar}
import cats.parse.{Parser => P, Parser0 => P0}

import parse.ast.*

val argument: P[(Identifier, Expr)] = id + (equals >> expr)
val argument_map : P0[Map[Identifier, Expr]] = list(argument).map(l => l.toMap)
val construction : P[Construction] = (id ~ (leftParen >> argument_map << rightParen)).map(x => 
    x match
        case (id: Identifier, args: Map[Identifier, Expr]) => Construction(id, args)
)

val object_declaration: P[ObjectDeclaration] = ((declaration << equals) + construction).map(
    x => x match
        case ((id: Identifier, t: Type), c: Construction) => ObjectDeclaration(id, t, c)
)

val subst: P0[Subst] = empty *> object_declaration.repSep0(lf ~ empty).map(Subst(_)) <* empty