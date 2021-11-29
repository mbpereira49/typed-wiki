def generate(b: Body): String = b match
  case Body.Blocks(l) => l.map(generateBlock).reduce((x, y) => s"$x\n$y")

def generateBlock(b : Block): String =
  b match {
      case Block.P(ls) => 
        val ls_gen = ls.map(generateLine).mkString("<br>\n")
        s"<p>$ls_gen</p>"
      case Block.Hdr(l, h) =>
        val l_gen = generateLine(l)
        val tag = h match {
            case Header.H1 => "h1"
            case Header.H2 => "h2"
            case Header.H3 => "h3"
        }
        s"<$tag>$l_gen</$tag>"
  }

def generateLine(l : Line): String = 
  l match
    case Line.Line_(l) => l.map(generateText).mkString

def generateText(t: Text): String =
  t match {
      case Text.Plain(s) => s
      case Text.Formatted(s, f) => 
        val tag = f match {
          case Format.Bold => "strong"
          case Format.Italics => "em"
          case _ => ""
        }
        s"<$tag>$s</$tag>"
  }
  