enum Header:
  case H1, H2, H3
enum Format:
  case Bold, Italics, Link

sealed trait Text
object Text:
  case class Plain(s: String) extends Text
  case class Bold(l: Line) extends Text
  case class Italics(l: Line) extends Text
  case class Link(l: Line, ref: String) extends Text

case class Line(l: Seq[Text])

sealed trait Block
object Block:
  case class P(ls: Seq[Line]) extends Block // paragraph
  case class Hdr(l: Line, h: Header) extends Block // header
  case class Meta(implements_list: List[Relation]) extends Block // template metadata

case class Template(l: List[Block])