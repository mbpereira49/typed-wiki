package parse.ast

case class ObjectDeclaration(id: Expr.Identifier, t: Type, c: Construction)
case class Construction(constructor: Expr.Identifier, args: Map[Expr.Identifier, Expr])

case class Subst(l: List[ObjectDeclaration])