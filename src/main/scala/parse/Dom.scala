import cats.parse.Rfc5234.{lf}
import cats.parse.{Parser => P, Parser0 => P0}

val declaration: P[(Expr.Identifier, Type)] = (identifier + (colon >> typeId))

val declaration_list: P[List[(Expr.Identifier, Type)]] = bracket_list(declaration)

val data: P[Data] = (P.string("data") >> (equals >> declaration_list)).map(l => Data(l.toMap))
val methods: P[Methods] = (P.string("methods") >> (equals >> declaration_list)).map(l => Methods(l.toMap))

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

val domain: P0[Domain] = empty *> definition.repSep0(lf ~ empty).map(Domain(_)) <* empty