package parse

import cats.parse.Rfc5234.{alpha, wsp, lf, digit, dquote, vchar, char}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.data.NonEmptyList

import ast.*
  
val id: P[Identifier] = {
  val id_char: P[Char] = alpha | P.charIn("_")
  val id: P[String] = id_char.rep.string
  id.map(Identifier(_))
}

val typeId: P[Type] = P.recursive[Type] { recurse =>
  def nest_functions(t: Type, l: NonEmptyList[Type]): Type.MapType =
    l match
      case NonEmptyList(hd, tl) => tl match {
        case Nil => Type.MapType(t, hd)
        case hd2 :: tl2 => Type.MapType(t, nest_functions(hd, NonEmptyList(hd2, tl2)))
      }
  val list_type = recurse.between(P.string("List["), P.string("]")).map(Type.ListType(_))
  val base = id.map(Type.Base(_))
  val tuple = paren_list(recurse).map(Type.TupleType(_))
  val parenthesized = leftParen >> recurse << rightParen
  val obj = parenthesized.backtrack | tuple.backtrack | list_type | base
  val mapping = (obj + (rightArrow >> obj).repSep(any_sp0)).map(x => x match
    case (t: Type, l: NonEmptyList[Type]) => nest_functions(t, l))

  mapping.backtrack | list_type | base
}

val empty: P0[Unit] = {
  val comment: P[Unit] = (P.string("//") ~ (vchar | wsp).rep0 ~ lf).void
  (comment | any_sp).rep0.void
}