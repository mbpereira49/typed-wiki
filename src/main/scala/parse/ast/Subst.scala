package parse.ast

case class ObjectDeclaration(id: Identifier, t: Type, c: Construction)
case class Construction(constructor: Identifier, args: Map[Identifier, Expr])

case class Subst(l: List[ObjectDeclaration])