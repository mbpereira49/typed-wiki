def generate(e: Expr): String = e match
  case Expr.Lines(l) => l.map(generateLine).reduce((x, y) => s"$x\n$y")

def generateLine(l : Line): String =
  l match {
      case Line.P(s) => s"<p>$s</p>"
      case Line.Hdr(h, s) =>
        val tag = h match {
            case Header.H1 => "h1"
            case Header.H2 => "h2"
            case Header.H3 => "h3"
        }
        s"<$tag>$s</$tag>"
  }