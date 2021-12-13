package generate

import parse.ast.*

def generate(s: Template, obj: types.Object = null): String = s match
  case Template(l) => 
    if l.isEmpty then ""
    else l.map(b => generateBlock(b, obj)).flatten.reduce((x, y) => s"$x\n$y")

def generateBlock(b : Block, obj: types.Object): Option[String] =
  b match {
      case Block.P(ls) => 
        val ls_gen = ls.map(l => generateLine(l, obj)).mkString("<br>\n")
        Some(s"<p>$ls_gen</p>")
      case Block.Hdr(l, h) =>
        val l_gen = generateLine(l, obj)
        val tag = h match {
            case Header.H1 => "h1"
            case Header.H2 => "h2"
            case Header.H3 => "h3"
        }
        Some(s"<$tag>$l_gen</$tag>")
      case Block.Meta(_) => None
  }

def generateLine(l : Line, obj: types.Object): String = 
  l match
    case Line(l) => l.map(t => generateText(t, obj)).mkString(" ")

def generateText(t: Text, obj: types.Object): String =
  t match {
      case Text.Plain(s) => s
      case Text.Bold(l) =>
        val l_gen = generateLine(l, obj)
        s"<strong>$l_gen</strong>"
      case Text.Italics(l) =>
        val l_gen = generateLine(l, obj)
        s"<em>$l_gen</em>"
      case Text.Link(line, link) =>
        val l_gen = generateLine(line, obj)
        s"<a href = $link>$l_gen</a>"
      case Text.Insert(e: parse.ast.Expr) => 
        val o: types.Object = eval.evalExpr(e, types.Env(), obj)
        o.toString

  }
  