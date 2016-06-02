package emailvalidator.parser

import emailvalidator.lexer._

import scala.util.parsing.combinator.Parsers

object EmailParser2 extends Parsers {
  type Elem = Token

  def apply(in: String) = all(new TokenReader(in))

  implicit def fromCondition(f:(TokenReader) => ParseResult[Token]): Parser[Token] = new Parser[Token] {
      override def apply(in: EmailParser2.Input): EmailParser2.ParseResult[Token] = body(in,f)
    }
  implicit def fromToken(token: Token): Parser[Token] = new Parser[Token] {
    override def apply(in: EmailParser2.Input): EmailParser2.ParseResult[Token] = {
      body(in, tr => {
        if (tr.first.equals(token)) Success(in.first, in)
        else Failure("f", in)
      })

    }
  }

  def body(in: Input, f:(TokenReader) => ParseResult[Token]) = {
    in match {
      case in:TokenReader => f(in)
      case _ => Error("no reader", in.rest)
    }
  }

  def local = opt(comment) | not(DOT()) ~ hasAt

  def comment: Parser[Token] = OPENPARENTHESIS() ~> Generic(_) <~ CLOSEPARENTHESIS()

  def hasAt: Parser[Token] = (tr:TokenReader) => {
    if (tr.realSource.contains(AT())) Success(tr.first, tr.rest)
    else Failure("no domain", tr.rest)
  }
  def domain: Parser[Token] = (tr:TokenReader) => {
    if (tr.realSource.tail.nonEmpty) Success(tr.first, tr.rest)
    else Failure("no domain", tr.rest)
  }

  def all: Parser[Token] = local ~> domain
}
