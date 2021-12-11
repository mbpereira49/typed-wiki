package eval

import parse.ast.{Expr, Lit, Attribute}

def evalExpr(e: Expr): types.Object =
    e match {
        /*case Expr.Plus(e1, e2) =>
            val v1 = evalExpr(e1)
            val v2 = evalExpr(e2)
            return v1 + v2*/
        case Expr.Literal(l) => evalLiteral(l)
        /*case Expr.Identifier(l) => evalVariable(id)*/
        case Expr.Access(e, a) => 
            val v = evalExpr(e)
            evalAccess(v, a)
    }


def identifierToString(id: Expr.Identifier): String =
    id match
        case Expr.Identifier(s) => s

def evalLiteral(l : Lit): types.Object =
    l match {
        case Lit.Number(i) => types.Int(i)
        case Lit.Str(s) => types.String(s)
    }

def evalAccess(e: types.Object, a: Attribute): types.Object =
    a match {
        case Attribute.Property(id: Expr.Identifier) =>
            val prop = identifierToString(id)
            e.state.values(prop)
        /*case Attribute.Method(id: Expr.Identifier, args: List[Expr]) => 
            eval_args: List[types.Object] = args.map(evalExpr)*/
    }