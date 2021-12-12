package eval

import parse.ast.{Expr, Lit, Attribute, Identifier}
import types.{Env, Object, Function, Property}

def evalExpr(e: Expr, env: Env, self: Object = null): Object =
    e match {
        case Expr.Plus(e1, e2) =>
            val v1 = evalExpr(e1, env, self)
            val plus = Attribute.Method(Identifier("__plus"), List(e2))
            return evalAccess(v1, plus, env, self)
        case Expr.Literal(l) => evalLiteral(l)
        case Expr.Var(id) => evalVar(id, env, self)
        case Expr.Access(e, a) => 
            val obj = evalExpr(e, env, self)
            evalAccess(obj, a, env, self)
    }

def evalLiteral(l : Lit): Object =
    l match {
        case Lit.Number(i) => types.Int(i)
        case Lit.Str(s) => types.String(s)
    }

def evalVar(id : Identifier, env: Env, self: Object): Object = 
    if env.mapping contains id then env.mapping(id)
    else id match
        case Identifier("self") => self
        case Identifier(s) => 
            throw Exception(s"Variable $s is undefined")

def evalAccess(obj: Object, a: Attribute, env: Env, self: Object): Object =
    a match {
        case Attribute.Property(id: Identifier) =>
            obj match {
                case obj: Property => obj.properties(id)
                case _ => throw Exception("Tried to access property of object with no Properties")
            }
        case Attribute.Method(id: Identifier, args: List[Expr]) => 
            val eval_args: List[Object] = args.map(e => evalExpr(e, env, self))
            val f : Function = obj.t.methods(id)
            f.run(obj)(env)(eval_args)
    }