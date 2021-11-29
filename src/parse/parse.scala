import cats.data.NonEmptyList
import cats.parse.Rfc5234.{alpha, sp, vchar, wsp, lf}
import cats.parse.{Parser => P, Parser0}
import cats.parse.Parser.not

enum Header:
  case H1, H2, H3
enum Format:
  case Bold, Italics, Link

sealed trait Text
object Text:
  case class Plain(s: String) extends Text
  case class Formatted(s: Line, f: Format) extends Text

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

val lf0: Parser0[Unit] = lf.rep0.void 
val wsp0: Parser0[Unit] = wsp.rep0.void 

val special = P.charIn("*_()[]")
val bold = P.string("*").as(Format.Bold)
val italics = P.string("_").as(Format.Italics)

val wordWithout: P[Any] => P[String] = p => (not(p).with1 ~ vchar).rep.string
val wordsWithout: P[Any] => P[String] = p => 
    wordWithout(p).repSep(wsp)
        .map(l => l.toList.mkString(" "))   

val normal_words = wordsWithout(special)

val line: P[Line] = P.recursive[Line] { recurse =>
    val formatted: P[Text] =
        (bold *> recurse <* bold).map(s => Text.Formatted(s, Format.Bold))
        | (italics *> recurse <* italics).map(s => Text.Formatted(s, Format.Italics))

    val plain: P[Text] = normal_words.map(s => Text.Plain(s))
    val text: P[Text] = formatted.backtrack | plain

    text.repSep(wsp0).map(l => Line.Line_(l.toList))
}

val paragraph: P[Block] = line.repSep(sp ~ sp ~ lf).map(l => Block.P(l.toList))

val header_marker: P[Header] = (
          P.string("###").as(Header.H3) 
        | P.string("##").as(Header.H2) 
        | P.string("#").as(Header.H1)
    ).surroundedBy(wsp0)

val header : P[Block] = ((header_marker <* wsp0) ~ line).map((h, l) => Block.Hdr(l, h))

val block : P[Block] = header | paragraph

val body : Parser0[Body] = block.repSep0(lf ~ lf).surroundedBy(lf0).map(l => Body.Blocks(l))