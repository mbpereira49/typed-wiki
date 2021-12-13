package parse

import cats.parse.Rfc5234.{alpha, sp, vchar, wsp, lf}
import cats.parse.{Parser => P, Parser0}
import cats.parse.Parser.not

import parse.ast.*

val lf0: Parser0[Unit] = lf.rep0.void 
val wsp0: Parser0[Unit] = wsp.rep0.void 

val special = P.charIn("*_()[]{}")
val bold = P.string("*").as(Format.Bold)
val italics = P.string("_").as(Format.Italics)

val link = (alpha | P.charIn("/:_.%&=")).rep.string

val wordWithout: P[Any] => P[String] = p => (not(p).with1 ~ vchar).rep.string
val wordsWithout: P[Any] => P[String] = p => 
    wordWithout(p).repSep(wsp)
        .map(l => l.toList.mkString(" "))   

val normal_words = wordsWithout(special)

val line: P[Line] = P.recursive[Line] { recurse =>
    val bold_line = (bold *> recurse <* bold).map(Text.Bold(_))
    val italics_line = (italics *> recurse <* italics).map(Text.Italics(_))
    val ref_line =
      ((leftBracket *> recurse <* rightBracket) ~ (leftParen *> link <* rightParen)).map(
        (line, link) => Text.Link(line, link)
      )
    val insert = (leftCurly >> expr << rightCurly).map(Text.Insert(_))
    val formatted: P[Text] = bold_line | italics_line | ref_line | insert

    val plain: P[Text] = normal_words.map(Text.Plain(_))

    val text: P[Text] = formatted.backtrack | plain

    text.repSep(wsp).map(l => Line(l.toList))
}

val paragraph: P[Block] = line.repSep(sp ~ sp ~ lf).map(l => Block.P(l.toList))

val header_marker: P[Header] = (
          P.string("###").as(Header.H3) 
        | P.string("##").as(Header.H2) 
        | P.string("#").as(Header.H1)
    ).surroundedBy(wsp0)

val header : P[Block] = ((header_marker <* wsp0) ~ line).map((h, l) => Block.Hdr(l, h))

val meta: P[Block] = ((leftBracket ~ leftBracket) >> implement0 << (rightBracket ~ rightBracket)).map(Block.Meta(_))

val block : P[Block] = header | meta | paragraph

val template : Parser0[Template] = block.repSep0(lf ~ lf).surroundedBy(lf0).map(Template(_))