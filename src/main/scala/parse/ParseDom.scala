import cats.parse.Rfc5234.{alpha, sp, char, wsp, lf, digit, cr, dquote, vchar}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.data.NonEmptyList

val any_sp = (wsp | lf | cr).void
val any_sp0 = (wsp | lf | cr).rep0.void
extension[A](p : P[A])
  def +[B](that: P0[B]) : P[(A, B)] = p ~ (any_sp0 *> that)
  def >>[B](that: P0[B]) : P[B] = p *> (any_sp0 *> that)
  def <<[B](that: P[B]) : P[A] = p <* (any_sp0 <* that)

extension[A](p : P0[A])
  def +[B](that: P0[B]) : P0[(A, B)] = p ~ (any_sp0 *> that)
  def >>[B](that: P0[B]) : P0[B] = p *> (any_sp0 *> that)
  def <<[B](that: P[B]) : P0[A] = p <* (any_sp0 <* that)

val id_char: P[Char] = alpha | P.charIn("_")
val id: P[String] = id_char.rep.string
val typeId: P[Type] = P.recursive[Type] { recurse =>
    recurse.between(P.string("List["), P.string("]")).map(Type.ListType(_)) | id.map(Type.Identifier(_))
}

val comma = P.char(',')
val colon = P.char(':')
val period = P.char('.')
val plus = P.char('+')
val equals = P.char('=')
val leftBracket = P.char('[')
val rightBracket = P.char(']')
val leftParen = P.char('(')
val rightParen = P.char(')')

val declaration: P[(String, Type)] = (id + (colon >> typeId))

def list[A] (p: P[A]): P0[List[A]] = p.repSep0(comma.surroundedBy(any_sp0).backtrack)
def bracket_list[A] (p : P[A]): P[List[A]] = leftBracket >> list(p) << rightBracket

val declaration_list: P[List[(String, Type)]] = bracket_list(declaration)

val data: P[Data] = (P.string("data") >> (equals >> declaration_list)).map(l => Data(l.toMap))
val methods: P[Methods] = (P.string("methods") >> (equals >> declaration_list)).map(l => Methods(l.toMap))

val number: P[Lit] = digit.rep.string.map(s => Lit.Number(s.toInt))
val valid_char: P[String] = alpha.string | digit.string | P.charIn(" !@#$%^&*()-_=+[]{};:<>,./?").string | P.string("\\\"").string.as("\"")
val string: P[Lit] = (dquote *> valid_char.repUntil0(dquote).map(_.mkString) <* dquote).map(s => Lit.Str(s))

def construct_accesses(e: Expr, l: NonEmptyList[Attribute]): Expr.Access =
  l match {
    case NonEmptyList(hd, tl) => tl match {
      case Nil => Expr.Access(e, hd)
      case hd2 :: tl2 => construct_accesses(Expr.Access(e, hd), NonEmptyList(hd2, tl2))
    }
  } 
val expr: P[Expr] = P.recursive[Expr] { recurse =>
  val identifier: P[Expr.Identifier] = id.map(x => Expr.Identifier(x))
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

val extend: P[List[Relation]] = (P.string("extends") >> list(id)).map(l => l.map(s => Extends(s)))
val implement: P[List[Relation]] = (P.string("implements") >> list(id)).map(l => l.map(s => Implements(s)))

def option_to_list[A](l : Option[List[A]]): List[A] =
  l match {
    case Some(l) => l
    case None => Nil
  }

val extend0: P0[List[Relation]] = extend.?.map(option_to_list)
val implement0: P0[List[Relation]] = implement.?.map(option_to_list)

val class_definition : P[ClassDef] = 
  (P.string("class") >> (id + (extend0 + implement0) + (equals >> fields)))
  .map(x => x match 
    case ((id: String, (e: List[Relation], i: List[Relation])), fields: List[Field]) =>
      ClassDef(id, e ++ i, fields)
  )

val interface_definition : P[InterfaceDef] =   
  (P.string("interface") >> (id + extend0 + (equals >> fields)))
  .map(x => x match 
    case ((id: String, i: List[Relation]), fields: List[Field]) =>
      InterfaceDef(id, i, fields)
  )

val definition: P[Definition] = class_definition | interface_definition

val comment: P[Unit] = (P.string("//") ~ (vchar | wsp).rep0 ~ lf).void

val empty: P0[Unit] = (comment | any_sp).rep0.void

//val domain: P0[Domain] = definition.repSep0((comment.backtrack | any_sp0).rep).map(Domain(_))
//val domain: P0[Domain] = (comment.repSep0(any_sp0).with1 *> definition).repSep0(lf ~ any_sp0).map(Domain(_)) <* comment.repSep0(any_sp0)
val domain: P0[Domain] = empty *> definition.repSep0(lf ~ empty).map(Domain(_)) <* empty