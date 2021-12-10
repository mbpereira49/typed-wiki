import cats.parse.Rfc5234.{alpha, sp, char, wsp, lf, digit, cr, dquote, vchar}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.data.NonEmptyList

val declaration: P[(Expr.Identifier, Type)] = (identifier + (colon >> typeId))

val declaration_list: P[List[(Expr.Identifier, Type)]] = bracket_list(declaration)

val data: P[Data] = (P.string("data") >> (equals >> declaration_list)).map(l => Data(l.toMap))
val methods: P[Methods] = (P.string("methods") >> (equals >> declaration_list)).map(l => Methods(l.toMap))

def construct_accesses(e: Expr, l: NonEmptyList[Attribute]): Expr.Access =
  l match {
    case NonEmptyList(hd, tl) => tl match {
      case Nil => Expr.Access(e, hd)
      case hd2 :: tl2 => construct_accesses(Expr.Access(e, hd), NonEmptyList(hd2, tl2))
    }
  } 
val expr: P[Expr] = P.recursive[Expr] { recurse =>
  val literal: P[Expr.Literal] = (number | string).map(Expr.Literal(_))
  val property: P[Attribute] = identifier.map(Attribute.Property(_))
  val method: P[Attribute] = (identifier ~ (leftParen >> list(recurse) << rightParen))
    .map(x => x match
      case (id: Expr.Identifier, e: List[Expr]) => Attribute.Method(id, e))
  val attribute: P[Attribute] = method.backtrack | property
  val access: P[Expr.Access] = (identifier ~ (period *> attribute).rep).map(x => x match 
      case (id: Expr.Identifier, l: NonEmptyList[Attribute]) => construct_accesses(id, l))
 
  val obj: P[Expr] = leftParen >> recurse << rightParen | access.backtrack | literal.backtrack | identifier
  val add: P[Expr.Plus] = ((obj << plus) + recurse).map(x => 
    x match 
      case (e1: Expr, e2: Expr) => Expr.Plus(e1, e2))
  add.backtrack | access.backtrack | literal.backtrack | identifier
}

val let: P[Field] = ((P.string("let") >> id) + (equals >> expr))
  .map(x => x match
    case (id: String, e: Expr) => Implementation(Expr.Identifier(id), e))

val field: P[Field] = data | methods | let
val fields: P[List[Field]] = bracket_list(field)

val extend: P[List[Relation]] = (P.string("extends") >> list(identifier)).map(l => l.map(s => Extends(s)))
val implement: P[List[Relation]] = (P.string("implements") >> list(identifier)).map(l => l.map(s => Implements(s)))

def option_to_list[A](l : Option[List[A]]): List[A] =
  l match {
    case Some(l) => l
    case None => Nil
  }

val extend0: P0[List[Relation]] = extend.?.map(option_to_list)
val implement0: P0[List[Relation]] = implement.?.map(option_to_list)

val class_definition : P[ClassDef] = 
  (P.string("class") >> (identifier + (extend0 + implement0) + (equals >> fields)))
  .map(x => x match 
    case ((id: Expr.Identifier, (e: List[Relation], i: List[Relation])), fields: List[Field]) =>
      ClassDef(id, e ++ i, fields)
  )

val interface_definition : P[InterfaceDef] =   
  (P.string("interface") >> (identifier + extend0 + (equals >> fields)))
  .map(x => x match 
    case ((id: Expr.Identifier, i: List[Relation]), fields: List[Field]) =>
      InterfaceDef(id, i, fields)
  )

val definition: P[Definition] = class_definition | interface_definition

val comment: P[Unit] = (P.string("//") ~ (vchar | wsp).rep0 ~ lf).void

val empty: P0[Unit] = (comment | any_sp).rep0.void

val domain: P0[Domain] = empty *> definition.repSep0(lf ~ empty).map(Domain(_)) <* empty