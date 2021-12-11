import cats.parse.Rfc5234.{alpha, sp, char, wsp, lf, digit, cr, dquote, vchar}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.data.NonEmptyList

import parse.ast.*

def nest_attributes(e: Expr, l: NonEmptyList[Attribute]): Expr.Access =
  l match
    case NonEmptyList(hd, tl) => tl match {
      case Nil => Expr.Access(e, hd)
      case hd2 :: tl2 => nest_attributes(Expr.Access(e, hd), NonEmptyList(hd2, tl2))
    }
  
val expr: P[Expr] = P.recursive[Expr] { recurse =>
  val literal: P[Expr.Literal] = (number | string).map(Expr.Literal(_))
  val variable = identifier.map(Expr.Var(_))
  val property: P[Attribute] = identifier.map(Attribute.Property(_))
  val method: P[Attribute] = (identifier ~ (leftParen >> list(recurse) << rightParen))
    .map(x => x match
      case (id: Identifier, e: List[Expr]) => Attribute.Method(id, e))
  val attribute: P[Attribute] = method.backtrack | property
  val value = variable | literal
  val access: P[Expr.Access] = (value ~ (period *> attribute).rep).map(x => x match 
      case (id: Expr, l: NonEmptyList[Attribute]) => nest_attributes(id, l))
 
  val obj: P[Expr] = leftParen >> recurse << rightParen | access.backtrack | literal.backtrack | variable
  val add: P[Expr.Plus] = ((obj << plus) + recurse).map(x => 
    x match 
      case (e1: Expr, e2: Expr) => Expr.Plus(e1, e2))
  add.backtrack | access.backtrack | literal.backtrack | variable
}