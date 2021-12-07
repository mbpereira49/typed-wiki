import cats.parse.Parser0

val filename = "plain_md"

val dir = "src/main/scala/test"
val fileIn = s"$dir/$filename.templ"
val fileOut = s"$dir/out/$filename.html" 

def parse_and_generate(p: Parser0[Template], fileIn: String, fileOut: String): Unit =
    val raw = read(fileIn)
    val parsed = p.parse(raw)
    write(parsed, fileOut)

@main def main =
    parse_and_generate(template, fileIn, fileOut)
    val parsed = class_definition.parse("class test extends X implements Y = [data = [hey: string], methods = [hi: int]]")
    parsed match {
      case Right(x) => println(x._2)
      case Left(x) => println(x)
    }