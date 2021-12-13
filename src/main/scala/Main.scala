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
    val domain_file = read("src/main/scala/test/simple.dom")
    val subst_file = read("src/main/scala/test/simple.subst")
    val parsed_domain = domain.parse(domain_file)
    val parsed_subst = subst.parse(subst_file)
    parsed_domain match {
        case Right(s, d) => 
            if !(s.isEmpty) then println(s"Unparsed: $s")
            else
                val tenv = eval.evalDom(d)
                parsed_subst match {
                    case Right(s, sub) => 
                        if !(s.isEmpty) then println(s"Unparsed: $s")
                        else
                            val e = eval.EvalSubst(tenv)
                            e.evalSubst(sub)
                    case Left(err) => println(err)
                }
        case Left(err) => println(err)
    }
