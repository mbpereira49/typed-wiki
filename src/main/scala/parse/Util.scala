import cats.parse.Rfc5234.{alpha, sp, char, wsp, lf, digit, cr, dquote, vchar}
import cats.parse.{Parser => P, Parser0 => P0}

val any_sp = (wsp | lf | cr).void
val any_sp0 = (wsp | lf | cr).rep0.void
extension[A](p : P[A])
  def +[B](that: P0[B]) : P[(A, B)] = p ~ (any_sp0 *> that)
  def >>[B](that: P0[B]) : P[B] = p *> (any_sp0 *> that)
  def <<[B](that: P[B]) : P[A] = p <* (any_sp0 <* that)

extension[A](p : P0[A])
  def +[B](that: P0[B]) : P0[(A, B)] = p ~ (any_sp0 *> that)
  def >>[B](that: P0[B]) : P0[B] = p *> (any_sp0 *> that)
  def <<[B](that: P[B]) : P0[A] = p <* (any_sp0 <* that)

val comma = P.char(',')
val colon = P.char(':')
val period = P.char('.')
val plus = P.char('+')
val equals = P.char('=')
val leftBracket = P.char('[')
val rightBracket = P.char(']')
val leftParen = P.char('(')
val rightParen = P.char(')')
val leftCurly = P.char('{')
val rightCurly = P.char('}')

def list[A] (p: P[A]): P0[List[A]] = p.repSep0(comma.surroundedBy(any_sp0).backtrack)
def bracket_list[A] (p : P[A]): P[List[A]] = leftBracket >> list(p) << rightBracket
def paren_list[A] (p : P[A]): P[List[A]] = leftParen >> list(p) << rightParen