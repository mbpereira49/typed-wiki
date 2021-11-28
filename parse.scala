import cats.data.NonEmptyList
import cats.parse.Rfc5234.{alpha, sp, vchar}
import cats.parse.{Parser => P, Parser0}

enum Header:
  case H1, H2, H3

sealed trait Line
object Line:
  case class P(s: String) extends Line // paragraph
  case class Hdr(h : Header, s: String) extends Line // header

sealed trait Expr
object Expr:
  case class Lines(l: List[Line]) extends Expr

val newline: P[Unit] = P.char('\n').void
val newline0: Parser0[Unit] = newline.rep0.void
val whitespace: P[Unit] = P.charIn(" \t").void
val whitespaces0: Parser0[Unit] = whitespace.rep0.void

val word: P[String] = vchar.rep.string
val text: P[String] = (word | whitespace).rep.string

val header: P[Header] = (
        P.string("###").as(Header.H3) 
        | P.string("##").as(Header.H2) 
        | P.string("#").as(Header.H1)
    ).surroundedBy(whitespaces0)

val line: P[Line] = (header ~ text).backtrack.map(x => Line.Hdr(x._1, x._2)) | text.map(x => Line.P(x))

val body: P[Expr] = line.surroundedBy(newline0).rep.map(l => Expr.Lines(l.toList))