package eval

import parse.ast.{Expr, Identifier, Domain, Definition, ClassDef, InterfaceDef, Data, Methods, Assignment, Implementation, Relation, Field}
import types.{Env, TypeEnv, Object, Function, Property, Type, IDType, ClassType, InterfaceType}
import scala.collection.mutable

def updateTenv(base: TypeEnv, add: TypeEnv): TypeEnv = 
    TypeEnv(base.mapping ++ add.mapping)

def parseDom(d: Domain, env: Env): TypeEnv =
    val tenv = TypeEnv()
    d match
        case Domain(l) => 
            for definition <- l do 
                val t = parseDefinition(definition, env, tenv)
                tenv.addType(t)
    tenv

def parseDefinition(d: Definition, env: Env, tenv: TypeEnv): IDType = 
    d match {
        case ClassDef(name, relations, fields) =>
            val (data, methods) = createMappings(relations, fields, tenv)
            ClassType(name, data, methods)
        case InterfaceDef(name, relation, fields) => null
    }

def createMappings(relations: Seq[Relation], fields: Seq[Field], tenv: TypeEnv):
    (Map[Identifier, types.Implementation], Map[Identifier, types.Implementation]) =           
    var data = mutable.Map[Identifier, types.Implementation]()
    var methods = mutable.Map[Identifier, types.Implementation]()
    for (field <- fields)
    do field match {
        case Data(m) => 
            val type_map = m.map((id, t) =>
                val c = getType(t, tenv)
                id -> types.Implementation.Unimplemented(c)
            )
            data++= type_map
        case Methods(m) =>
            val type_map = m.map((id, t) =>
                val c = getType(t, tenv)
                id -> types.Implementation.Unimplemented(c)
            )
            methods ++= type_map
        case Assignment(id, expr) =>
            if data contains id then
                val t = types.getType(data(id))
                val obj = types.LazyObject(expr)
                val impl = types.Implementation.Implemented(t, obj)
                data += (id -> impl)
            else throw Exception("Cannot assign undeclared variable")
        case Implementation(id, args, expr) => 
            if methods contains id then
                val t = types.getType(methods(id))
                val arg_map = args.map((id, t) => id -> getType(t, tenv))
                val obj = types.Method(arg_map, expr)
                val impl = types.Implementation.Implemented(t, obj)
                data += (id -> impl)
            else throw Exception("Cannot implement undeclared method") 
    }
    (data.toMap, methods.toMap)

def getType(t: parse.ast.Type, tenv: TypeEnv): types.Type =
    t match {
        case parse.ast.Type.Identifier(s) => 
            val id = Identifier(s)
            if tenv.mapping contains id then tenv.mapping(id) 
            else throw Exception(s"Type $id not defined")
        case parse.ast.Type.ListType(t) =>
            val c = getType(t, tenv)
            types.ListType(c)
        case parse.ast.Type.MapType(t1, t2) =>
            val c1 = getType(t1, tenv)
            val c2 = getType(t2, tenv)
            types.MapType(c1, c2)
        case parse.ast.Type.TupleType(ts) =>
            val cs = ts.map(t => getType(t, tenv))
            types.TupleType(cs)
    }