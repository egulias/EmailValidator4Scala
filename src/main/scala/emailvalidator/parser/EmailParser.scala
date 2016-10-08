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

  def local: Parser[Token] = (log(dquote)("dquoute") | log(rep(atom ~> DOT()))("repeat atom") ~> atom ~> log(comment)("comment")) <~ log(AT())("finding @")

  def comment: Parser[Token] =
    opt(OPENPARENTHESIS()) into(op => op match {
      case Some(x) => rep(log(atom)("comment atom")) ~> CLOSEPARENTHESIS()
      case None => Parser{ in => Success(null, in) }
    })

  def dquote: Parser[Token] = log(DQUOTE())("open quote") ~> opt(rep(log(atom)("double quote atom"))) ~> log(DQUOTE())("closing quote")

  def domain = phrase(log(domainAtom)("domain atom") ~> log(rep(DOT() ~> domainAtom))("repeat domain atom"))

  def atom = acceptIf {
    case _: GENERIC => true
    case _ => false
  }(t => s"failed at $t")

  def domainAtom = acceptIf {
      case c: GENERIC => c.isAscii
      case _ => false
    }(t => s"failed at $t")

  def hasAt: Parser[Token] = (tr: TokenReader) => {
    if (tr.realSource.contains(AT())) Success(tr.first, tr.rest)
    else Failure("no domain", tr.rest)
  }
}
