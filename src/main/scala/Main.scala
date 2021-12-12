import cats.parse.Parser0

import parse.ast.*

val filename = "simple"

val dir = "src/main/scala/test"
val fileIn = s"$dir/$filename.templ"
val fileOut = s"$dir/out/$filename.html" 

def parse_and_generate(p: Parser0[Template], fileIn: String, fileOut: String): Unit =
    val raw = read(fileIn)
    val parsed = p.parse(raw)
    write(parsed, fileOut)

@main def main =
    parse_and_generate(template, fileIn, fileOut)
    val input = read("src/main/scala/test/test.dom")
    val parsed = expr.parse(input)
    println(parsed)
    parsed match {
        case Right(s, e) => 
            if !(s.isEmpty) then println(s"Unparsed: $s") else ()
            println("Output: " + eval.evalExpr(e, types.Env()).toString)
        case Left(err) => println(err)
    }
