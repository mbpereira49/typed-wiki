package run

def run(baseDir: String, filenameDom: String, filenameSubst: String): Unit =     
    val domainFile = io.read(s"$baseDir/$filenameDom")
    val substFile = io.read(s"$baseDir/$filenameSubst")

    val parsedDomain = parse.domain.parseAll(domainFile)
    val parsedSubst = parse.subst.parseAll(substFile)

    val dom = extractDomain(parsedDomain)
    val subst = extractSubst(parsedSubst)

    val tenv = eval.evalDom(dom)

    val e = eval.EvalSubst(tenv)
    e.evalSubst(subst)

    val outDir = s"$baseDir/out"
    e.getPages().foreach(p => writePage(p, outDir))
    writeCSS(outDir)

private def writePage(page: types.Object, outDir: String): Unit = 
    val path = eval.EvalSubst.generatePath(page)
    val html = eval.EvalSubst.generateHTML(page)
    val fileOut = s"$outDir/html/$path.html"
    io.write_html(html, fileOut)

private def writeCSS(outDir: String): Unit =
    val cssFiles = generate.getCSSFiles
    cssFiles.foreach(f => io.copy(f, s"$outDir/css"))

private def extractDomain[A](e: Either[cats.parse.Parser.Error, A]): A =
    extract(e, "dom")

private def extractSubst[A](e: Either[cats.parse.Parser.Error, A]): A =
    extract(e, "subst")

private def extract[A](e: Either[cats.parse.Parser.Error, A], context: String): A =
    e match
        case Right(a) => a
        case Left(err) => throw Exception(s"Error parsing $context: $err")