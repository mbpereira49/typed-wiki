package eval

import parse.ast.{Subst, ObjectDeclaration, Construction, Identifier, Expr}
import types.{Env, TypeEnv, ClassType, InterfaceType, Implementation, ClassInstance}

class EvalSubst(val tenv: TypeEnv, var env: Env = Env()):
    def getPages(): Iterable[types.Object] =
        env.mapping.filter((id, o) => 
            isSubtype(env.typeMapping(id), tenv.mapping(Identifier("Page"))))
        .map((id, o) => o)

    def evalSubst(s: Subst): Unit =
        s match
            case Subst(l : List[ObjectDeclaration]) => l.map(evalObjectDeclaration)

    def evalObjectDeclaration(o: ObjectDeclaration): Unit =
        o match
            case ObjectDeclaration(id: Identifier, t: parse.ast.Type, c: Construction) =>
                val obj: types.Object = evalConstruction(c)
                val decl_t : types.Type = types.getType(t, tenv)
                if isSubtype(obj.t, decl_t) then 
                    env.add(id, obj, decl_t)
                else 
                    val found_t = obj.t
                    throw Exception(s"Expected object of type $decl_t, found of type $found_t")

    def evalConstruction(c: Construction): types.Object = 
        c match
            case Construction(id: Identifier, args: Map[Identifier, Expr]) =>
                val t : types.Type = 
                    if tenv.mapping contains id then tenv.mapping(id)
                    else throw Exception(s"Constructor $id is not found in type environment")
                t match {
                    case (c: ClassType) =>
                        val properties : Map[Identifier, types.Object] = c.data.map((id, impl) =>
                            if args contains id then id -> (eval.evalExpr(args(id), env))
                            else impl match {
                                case Implementation.Implemented(t, obj) => 
                                    val found = obj.t
                                    // Should I be discarding t here?
                                    if isSubtype(found, t) then id -> obj
                                    else throw Exception(s"Object $id expected type $t, found $found")
                                case Implementation.Unimplemented(t) => throw Exception(s"Object field $id unimplemented")
                            }
                        )
                        ClassInstance(c, properties)
                    case (_: InterfaceType) => throw Exception("Cannot instantiate interface")
                    // consider using a trait to detect when something cannot be instantiated
                    case _ => throw Exception("Unimplemented construction")
                }

def isSubtype(lower: types.Type, upper: types.Type): Boolean =
    (lower eq upper) | lower.parents.map(p => isSubtype(p, upper)).exists(b => b)

object EvalSubst:
    def generateHTML(obj: types.Object): String = 
        val expr_string = "this.render_full()"
        val html = eval.evalExprString(expr_string, Env(), obj)
        extractString(html)
    
    def generatePath(obj: types.Object): String =
        val expr_string = "this.path()"
        val path = eval.evalExprString(expr_string, Env(), obj)
        extractString(path)

    private def extractString(obj: types.Object): String =
        obj match
            case (obj: types.String) => obj.value
            case _ => throw Exception("Link for page is not of type String")