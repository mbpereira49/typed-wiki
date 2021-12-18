import cats.parse.Parser0

import parse.ast.*
import parse.*
import io.*

val dir = "src/main/scala/test"

val filenameDom = "simple"
val filenameSubst = "simple"

def writePage(page: types.Object): Unit = 

    val link = eval.EvalSubst.generateLink(page)
    val filename = link match {
        case (s: types.String) => s.value
        case _ => throw Exception("Link for page is not of type String")
    }
    val html = eval.EvalSubst.generateHTML(page)
    html match {
        case (s: types.String) =>
            val fileOut = s"$dir/out/$filename.html"
            io.write_html(s.value, fileOut)
        case _ => 
            println("Rendering did not generate String")
    }

@main def main =
    val domainFile = read(s"$dir/$filenameDom.dom")
    val substFile = read(s"$dir/$filenameSubst.subst")
    val parsedDomain = domain.parse(domainFile)
    val parsedSubst = subst.parse(substFile)
    parsedDomain match {
        case Right(s, d) => 
            if !(s.isEmpty) then println(s"Unparsed: $s")
            else
                val tenv = eval.evalDom(d)
                parsedSubst match {
                    case Right(s, sub) => 
                        if !(s.isEmpty) then println(s"Unparsed: $s")
                        else
                            val e = eval.EvalSubst(tenv)
                            e.evalSubst(sub)
                            val pages: Iterable[types.Object] = e.getPages()
                            pages.foreach(writePage)
                    case Left(err) => println(err)
                }
        case Left(err) => println(err)
    }
