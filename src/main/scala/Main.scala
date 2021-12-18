import cats.parse.Parser0

import parse.ast.*
import parse.*
import io.*

val dir = "src/main/scala/test"

def writePage(page: types.Object): Unit = 
    val path = eval.EvalSubst.generatePath(page)
    val filename = path match {
        case (path: types.String) => path.value
        case _ => throw Exception("Link for page is not of type String")
    }

    val html = eval.EvalSubst.generateHTML(page)
    html match {
        case (html: types.String) =>
            val fileOut = s"$dir/out/$filename.html"
            io.write_html(html.value, fileOut)
        case _ => 
            println("Rendering did not generate String")
    }

@main def main(filenameDom: String, filenameSubst: String) =
    val domainFile = read(s"$dir/$filenameDom")
    val substFile = read(s"$dir/$filenameSubst")
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
