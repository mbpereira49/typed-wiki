package eval

import parse.ast.{Subst, ObjectDeclaration, Construction, Identifier, Expr}
import types.{Env, TypeEnv, ClassType, InterfaceType, Implementation, ClassInstance}

class EvalSubst(val tenv: TypeEnv, var env: Env = Env()):
    def evalSubst(s: Subst): Unit =
        s match
            case Subst(l : List[ObjectDeclaration]) =>
                val objs: List[Unit] = l.map(evalObjectDeclaration)

    def evalObjectDeclaration(o: ObjectDeclaration): Unit =
        o match
            case ObjectDeclaration(id: Identifier, t: parse.ast.Type, c: Construction) =>
                val obj: types.Object = evalConstruction(c)
                val decl_t : types.Type = types.getType(t, tenv)
                // Again should not discard decl_t here, need a new mapping in the environment
                if isSubtype(obj.t, decl_t) then env.add(id, obj)
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
                            impl match {
                                case Implementation.Implemented(t, obj) => 
                                    val found = obj.t
                                    // Should I be discarding t here?
                                    if isSubtype(found, t) then id -> obj
                                    else throw Exception(s"Object $id expected subtype of type $t, found $found")
                                case Implementation.Unimplemented(t) => throw Exception(s"Object field $id unimplemented")
                            }
                        )
                        ClassInstance(c, properties)
                    case (_: InterfaceType) => throw Exception("Cannot instantiate interface")
                    // consider using a trait to detect when something cannot be instantiated
                    case _ => throw Exception("Unimplemented construction")
                }

def isSubtype(lower: types.Type, upper: types.Type): Boolean =
    // Need to change to do subtyping!
    lower eq upper