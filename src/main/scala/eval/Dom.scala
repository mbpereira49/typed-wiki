package eval

import parse.ast.{Expr, Identifier, Domain, Definition, ClassDef, InterfaceDef, Data, Methods, Assignment, Implementation, Field, Relation, Extends, Implements}
import types.{Env, TypeEnv, Object, Function, Property, Type, IDType, StateType, ClassType, InterfaceType}
import scala.collection.mutable

def initializeTenv(): TypeEnv =
    val tenv = TypeEnv()
    tenv.addType(types.Unit.t)
    tenv.addType(types.String.t)
    tenv.addType(types.Int.t)
    tenv.addType(types.Class.t)
    tenv.addType(types.Template.t)
    tenv.addType(types.STL.t)
    tenv

def evalDom(d: Domain): TypeEnv =
    val tenv = initializeTenv()
    d match
        case Domain(l) => 
            for definition <- l do 
                val t = constructDefinition(definition, tenv)
                tenv.addType(t)
    tenv

def constructDefinition(d: Definition, tenv: TypeEnv): IDType = 
    d match {
        case ClassDef(name, relations, fields) =>
            val (data, methods) = constructFields(relations, fields, tenv)
            val parents : Seq[Type] = relations.filter(_.isInstanceOf[Extends]).map(r => getRelationType(r, tenv)).flatten
            val interfaces : Seq[Type] = relations.filter(_.isInstanceOf[Implements]).map(r => getRelationType(r, tenv)).flatten
            val refined_parents : Seq[ClassType] = parents.map(refineToClass)
            val refined_interfaces : Seq[InterfaceType] = interfaces.map(refineToInterface)
            ClassType(name, data, methods, refined_parents, refined_interfaces)
        case InterfaceDef(name, relations, fields) =>
            val parents = relations.filter(_.isInstanceOf[Extends]).map(r => getRelationType(r, tenv)).flatten
            val (data, methods) = constructFields(relations, fields, tenv)
            val refined_parents : Seq[InterfaceType] = parents.map(refineToInterface)
            InterfaceType(name, data, methods, refined_parents)
    }

def constructFields(relations: Seq[Relation], fields: Seq[Field], tenv: TypeEnv):
    (Map[Identifier, types.Implementation], Map[Identifier, types.Implementation]) =           
    var data = mutable.Map[Identifier, types.Implementation]()
    var methods = mutable.Map[Identifier, types.Implementation]()

    for (r <- relations) do
        getRelationType(r, tenv) match {
            case Some(t) => t match {
                case (t : StateType) =>
                    data ++= t.data
                    methods ++= t.methods
                case (t : Type) =>
                    methods ++= t.methods
            }
            case None => throw Exception(s"class/interface has relation $r on undefined class/interface")
        }

    for (field <- fields)
    do field match {
        case Data(m) => 
            val type_map = m.map((id, t) =>
                val c = types.getType(t, tenv)
                id -> types.Implementation.Unimplemented(c)
            )
            data++= type_map
        case Methods(m) =>
            val type_map = m.map((id, t) =>
                val c = types.getType(t, tenv)
                id -> types.Implementation.Unimplemented(c)
            )
            methods ++= type_map
        case Assignment(id, expr) =>
            if data contains id then
                val t = types.getImplementationType(data(id))
                val obj = eval.evalExpr(expr, Env())
                val impl = types.Implementation.Implemented(t, obj)
                data += (id -> impl)
            else throw Exception(s"Cannot assign undeclared variable $id")
        case Implementation(id, args, expr) => 
            if methods contains id then
                val t = types.getImplementationType(methods(id))
                val arg_map = args.map((id, t) => id -> types.getType(t, tenv))
                val obj = types.Method(arg_map, expr)
                val impl = types.Implementation.Implemented(t, obj)
                methods += (id -> impl)
            else throw Exception(s"Cannot implement undeclared method $id") 
    }
    (data.toMap, methods.toMap)

def getRelationType(r: Relation, tenv: TypeEnv): Option[Type] =
    val id = r match {
        case Extends(s) => s
        case Implements(s) => s
    }
    if tenv.mapping contains id then Some(tenv.mapping(id))
    else None

def refineToClass(s: Type): ClassType =
    s match {
        case (c: ClassType) => c
        case _ => throw Exception(s"$s of type Type should be of type ClassType")
    }
def refineToInterface(s: Type): InterfaceType =
    s match {
        case (c: InterfaceType) => c
        case _ => throw Exception(s"$s of type Type should be of type InterfaceType")
    }