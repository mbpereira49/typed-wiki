package parse

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

  val number: P[Lit] = digit.rep.string.map(s => Lit.Number(s.toInt))
  val string: P[Lit] = {
    val escape: P[String] = P.string("\\\"").string.as("\"") | P.string("\\n").string.as("\n")
    val valid_char: P[String] = 
      escape.backtrack
      | char.string
    (dquote *> valid_char.repUntil0(dquote).map(_.mkString) <* dquote).map(s => Lit.Str(s))
  }
  val literal: P[Expr.Literal] = (number | string).map(Expr.Literal(_))
  val variable = id.map(Expr.Var(_))
  val property: P[Attribute] = id.map(Attribute.Property(_))
  val method: P[Attribute] = (id ~ (leftParen >> list(recurse) << rightParen))
    .map(x => x match
      case (id: Identifier, e: List[Expr]) => Attribute.Method(id, e))
  val attribute: P[Attribute] = method.backtrack | property
  val value = leftParen >> recurse << rightParen | variable | literal
  val access: P[Expr.Access] = (value ~ (period *> attribute).rep).map(x => x match 
      case (id: Expr, l: NonEmptyList[Attribute]) => nest_attributes(id, l))
 
  val obj: P[Expr] = access.backtrack | leftParen >> recurse << rightParen | literal.backtrack | variable
  val add: P[Expr.Plus] = ((obj << plus) + recurse).map(x => 
    x match 
      case (e1: Expr, e2: Expr) => Expr.Plus(e1, e2))
  add.backtrack | obj
}