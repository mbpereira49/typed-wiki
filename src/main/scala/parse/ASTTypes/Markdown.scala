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

sealed trait Line
object Line:
  case class Line_(l: Seq[Text]) extends Line

sealed trait Block
object Block:
  case class P(ls: Seq[Line]) extends Block // paragraph
  case class Hdr(l: Line, h: Header) extends Block // header

sealed trait Body
object Body:
  case class Blocks(l: List[Block]) extends Body