package emailvalidator.parser

import emailvalidator.lexer._

import scala.util.parsing.combinator.Parsers

object EmailParser extends Parsers {
  type Elem = Token

  implicit def fromCondition(f: (TokenReader) => ParseResult[Token]): Parser[Token] = new Parser[Token] {
    override def apply(in: EmailParser.Input): EmailParser.ParseResult[Token] = body(in, f)
  }

  def body(in: Input, f: (TokenReader) => ParseResult[Token]) = {
    in match {
      case in: TokenReader => f(in)
      case _ => Error("no reader", in.rest)
    }
  }

  def local = (log(dquote)("dquoute") | log(rep(atom ~> opt(DOT() ~> atom)))("repeat atom") <~ (log(escaped)("escaped") | log(comment)("comment2")) | Parser{in=>Success(in.first, in.rest)}) <~ log(AT())("finding @")

  def atom = acceptIf {
    case _: GENERIC => true
    case _ => false
  }(t => s"failed at $t")


  def comment: Parser[Token] = OPENPARENTHESIS() ~> (atom | comment) <~ CLOSEPARENTHESIS()

  def dquote: Parser[Token] = log(DQUOTE())("open quote") ~> opt(rep(log(atom)("double quote atom"))) ~> log(DQUOTE())("closing quote")

  def escaped = log(BACKSLASH())("escaped backslash") ~ log(consumeTokenNot(atom))("not atom") ~> atom

  private def consumeTokenNot[T](p: => Parser[T]): Parser[Unit] = Parser { in =>
    p(in) match {
      case Success(_, _) => Failure("Expected failure", in)
      case _ => Success(in.first, in.rest)
    }
  }

  def domain = phrase(log(domainAtom)("domain atom") ~> log(rep(DOT() ~> domainAtom))("repeat domain atom"))

  def domainAtom = acceptIf {
    case c: GENERIC => c.isAscii
    case _ => false
  }(t => s"failed at $t")

  def hasAt: Parser[Token] = (tr: TokenReader) => {
    if (tr.tokenizedSource.contains(AT())) Success(tr.first, tr.rest)
    else Failure("no domain", tr.rest)
  }
}
