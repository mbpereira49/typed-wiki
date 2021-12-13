package eval

import parse.ast.{Subst, ObjectDeclaration, Construction, Identifier, Expr}
import types.{Env, TypeEnv, ClassType, InterfaceType, Implementation, ClassInstance}

class EvalSubst(val tenv: TypeEnv, var env: Env = Env()):
    def evalSubst(s: Subst): Unit =
        s match
            case Subst(l : List[ObjectDeclaration]) => l.map(evalObjectDeclaration)
        env.mapping.map((id, o) =>
            if !isSubtype(env.typeMapping(id),tenv.mapping(Identifier("Page"))) then () 
            else
                val expr_string = "self.render_full()"
                val expr_parsed = parse.expr.parse(expr_string)
                expr_parsed match {
                    case Right(s, e) => 
                        val output = eval.evalExpr(e, Env(), o)
                        output match {
                            case (s: types.String) =>
                                val filename = "temp"
                                val dir = "src/main/scala/test"
                                val fileIn = s"$dir/$filename.templ"
                                val fileOut = s"$dir/out/$filename.html" 
                                io.write_html(s.value, fileOut)
                            case _ => 
                                println("Rendering did not generate String")
                        }
                    case Left(_) => throw Exception("Parsing error")
                }
        )
        
        

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