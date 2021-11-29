def generate(e: Body): String = e match
  case Body.Blocks(l) => l.map(generateLine).reduce((x, y) => s"$x\n$y")

def generateLine(b : Block): String =
  b match {
      case Block.P(s) => s"<p>$s</p>"
      case Block.Hdr(s, h) =>
        val tag = h match {
            case Header.H1 => "h1"
            case Header.H2 => "h2"
            case Header.H3 => "h3"
        }
        s"<$tag>$s</$tag>"
  }