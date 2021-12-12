package types

import parse.ast.{Identifier, Expr}

trait Function extends Object:
    def run(self: Object)(env: Env)(objs: List[Object]): Object


class Method(val args: Map[Identifier, Type], val body: Expr, val t2: parse.ast.Type) extends Function:
    val t = null//MapType(ListType(args.values.toList), body.t)
    def run(self: Object)(env: Env)(objs: List[Object]): Object =
        val bindings: Map[Identifier, Object] = (args.keys zip objs).toMap
        val new_env: Env = Env(env.mapping ++ bindings)
        eval.evalExpr(body, new_env)

class NativeFunction(f: Object => List[Object] => Object, val t: Type) extends Function:
    def run(self: Object)(env : Env)(objs: List[Object]): Object = f(self)(objs)