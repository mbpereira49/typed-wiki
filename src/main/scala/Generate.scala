def generate(s: Template): String = s match
  case Template(l) => 
    if l.isEmpty then ""
    else l.map(generateBlock).flatten.reduce((x, y) => s"$x\n$y")

def generateBlock(b : Block): Option[String] =
  b match {
      case Block.P(ls) => 
        val ls_gen = ls.map(generateLine).mkString("<br>\n")
        Some(s"<p>$ls_gen</p>")
      case Block.Hdr(l, h) =>
        val l_gen = generateLine(l)
        val tag = h match {
            case Header.H1 => "h1"
            case Header.H2 => "h2"
            case Header.H3 => "h3"
        }
        Some(s"<$tag>$l_gen</$tag>")
      case Block.Meta(_) => None
  }

def generateLine(l : Line): String = 
  l match
    case Line(l) => l.map(generateText).mkString(" ")

def generateText(t: Text): String =
  t match {
      case Text.Plain(s) => s
      case Text.Bold(l) =>
        val l_gen = generateLine(l)
        s"<strong>$l_gen</strong>"
      case Text.Italics(l) =>
        val l_gen = generateLine(l)
        s"<em>$l_gen</em>"
      case Text.Link(line, link) =>
        val l_gen = generateLine(line)
        s"<a href = $link>$l_gen</a>"
      case Text.Insert(e) => "{null}"

  }
  