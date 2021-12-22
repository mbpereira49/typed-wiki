package types

import parse.ast.Identifier
import types.Function
import types.Implementation.Implemented

sealed trait Value:
    val value: Any
    override def toString: Predef.String = value.toString
    

object Unit:
    val t = BaseType(Identifier("Unit"), Map())
class Unit extends Object, Value:
    val value: scala.Unit = ()
    val t = Unit.t

object String:
    val plus: NativeFunction = NativeFunction(self => objs =>
            scala.Console.flush()
            (self, objs) match {
                case (_, Nil) => throw Exception("'+' takes second argument")
                case (self: types.String, (that : types.String) :: tl) => types.String(self.value + that.value)
                case _ => throw Exception("Type of second argument to '+' does not match String")
            },
            MapType(t, t)
        )
    val methods= Map[Identifier, types.Implementation](Identifier("__plus") -> Implemented(MapType(t, t), plus))
    val t: BaseType = BaseType(Identifier("String"), methods)
class String(s: Predef.String) extends Object, Value:
    val value: Predef.String = s
    val t = String.t

object Int:
    val toStr = NativeFunction(self => lst => 
        self match {
            case (self: Value) => types.String(self.value.toString)
            case _ => throw Exception("self not Value. Should not reach this but don't know how to guarantee")
        },
        MapType(Unit.t, String.t)
    )
    val plus: NativeFunction = NativeFunction(self => objs =>
        (self, objs) match {
            case (_, Nil) => throw Exception("'+' takes second argument")
            case (self: types.Int, (that : types.Int) :: Nil) => types.Int(self.value + that.value)
            case _ => throw Exception("Type of second argument to '+' does not match Int")
        },
        MapType(t, t)
    )
    val methods = Map[Identifier, types.Implementation](
        Identifier("__plus") -> Implemented(MapType(t, t), plus),
        Identifier("string") -> Implemented(MapType(t, t), toStr)
    )

    val t: BaseType = BaseType(Identifier("Int"), methods)
class Int(i: scala.Int) extends Object, Value:
    val value: scala.Int = i
    val t = Int.t

// need any ClassType to be subtype of this
object Class:
    val t = BaseType(Identifier("Class"), Map())
class Class extends Object:
    val t = Class.t

object Template:
    val generate_method = NativeFunction(self => objs =>
        (self, objs) match {
            case (_, Nil) => throw Exception("generate requires an object")
            case (self: types.Template, (obj : types.Object) :: Nil) => types.String(generate.generateHTML(self.value, obj))
            case _ => throw Exception("Too many arguments")
        },
        MapType(types.Class.t, String.t)
    )
    val methods = Map(Identifier("generate") -> Implemented(MapType(types.Class.t, String.t), generate_method))
    val t: BaseType = BaseType(Identifier("Template"), methods)
class Template(templ: parse.ast.Template) extends Object, Value:
    val t = Template.t
    val value: parse.ast.Template = templ

object STL:
    val read_template = NativeFunction(self => objs => 
        (self, objs) match {
            case (_, Nil) => throw Exception("read_template requires an object")
            case (self: types.STL, (file : types.String) :: Nil) => 
                val filename = file.value
                val str_contents = io.read(filename)
                val templ = parse.template.parse(str_contents)
                templ match {
                    case Right(x) => 
                        types.Template(x._2)
                    case Left(x) => throw Exception (s"Parsing error when parsing $filename")
                }
                
            case _ => throw Exception("Argument is not string or too many arguments")
        },
        MapType(String.t, Template.t)
    )
    val methods = Map(Identifier("read_template") -> Implemented(MapType(String.t, Template.t), read_template))
    val t: BaseType = BaseType(Identifier("STL"), methods)
class STL extends Object:
    val t = STL.t
