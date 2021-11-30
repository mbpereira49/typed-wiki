import cats.parse.Parser

val filename = "test1"

val dir = "src/main/scala/test"
val fileIn = s"$dir/$filename.subst"
val fileOut = s"$dir/out/$filename.html" 

def parse_and_generate[A](parser: Parser[A], PfileIn: String, fileOut: String): Unit =
    val raw = read(fileIn)
    val parsed = substance.parse(raw)
    write(parsed, fileOut)

@main def hello =
    val fileContents = read_markdown("src/main/scala/test/test1.dom")
    val parsed = type_.parse(fileContents)
    println(parsed)