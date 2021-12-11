package eval

import parse.ast.{Expr, Lit, Attribute, Identifier}
import types.{Env, Object, Function, Property}

def evalExpr(e: Expr, env: Env): Object =
    e match {
        case Expr.Plus(e1, e2) =>
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            return v1
        case Expr.Literal(l) => evalLiteral(l)
        case Expr.Var(id) => evalVar(id, env)
        case Expr.Access(e, a) => 
            val v = evalExpr(e, env)
            evalAccess(v, a, env)
    }

def evalLiteral(l : Lit): Object =
    l match {
        case Lit.Number(i) => types.Int(i)
        case Lit.Str(s) => types.String(s)
    }

def evalVar(id : Identifier, env: Env): Object = 
    if env.mapping contains id then env.mapping(id)
    else id match
        case Identifier(s) => 
            throw Exception(s"Variable $s is undefined")

def evalAccess(obj: Object, a: Attribute, env: Env): Object =
    a match {
        case Attribute.Property(id: Identifier) =>
            obj match {
                case obj: Property => obj.properties(id)
                case _ => throw Exception("Tried to access property of object with no Properties")
            }
        case Attribute.Method(id: Identifier, args: List[Expr]) => 
            val eval_args: List[Object] = args.map(e => evalExpr(e, env))
            val f : Function = obj.t.methods(id)
            val bindings: Map[Identifier, Object] = (f.args.keys zip eval_args).toMap
            val new_env: Env = Env(env.mapping ++ bindings)
            evalExpr(f.body, new_env)
    }