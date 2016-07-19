package emailvalidator.parser

import emailvalidator.lexer._

import scala.util.parsing.combinator.Parsers

object EmailParser2 extends Parsers {
  type Elem = Token

  def apply(in: String) = all(new TokenReader(in))

  implicit def fromCondition(f: (TokenReader) => ParseResult[Token]): Parser[Token] = new Parser[Token] {
    override def apply(in: EmailParser2.Input): EmailParser2.ParseResult[Token] = body(in, f)
  }

  implicit def fromToken(token: Token): Parser[Token] = new Parser[Token] {
    override def apply(in: EmailParser2.Input): EmailParser2.ParseResult[Token] = {
      body(in, tr => {
        if (tr.first.equals(token)) Success(in.first, in.rest)
        else Failure(s"failed at ${in.first}", in)
      })

    }
  }

  def body(in: Input, f: (TokenReader) => ParseResult[Token]) = {
    in match {
      case in: TokenReader => f(in)
      case _ => Error("no reader", in.rest)
    }
  }

  def local: Parser[Token] = rep1(log(atom)("local part atom")) ~> comment <~ log(AT())("finding @")

  def comment = acceptIf {
      case _: OPENPARENTHESIS => true
      case _ => false
    }(t => "no comment").into(e => log(rep(log(atom)("comment atom")))("rep") <~ log(CLOSEPARENTHESIS())("close paren") into (l => l.last))


  def atom = acceptIf {
    case _: GENERIC => true
    case _ => false
  }(t => s"failed at $t")

  def hasAt: Parser[Token] = (tr: TokenReader) => {
    if (tr.realSource.contains(AT())) Success(tr.first, tr.rest)
    else Failure("no domain", tr.rest)
  }

  def domain: Parser[Token] = (tr: TokenReader) => {
    if (tr.realSource.tail.nonEmpty) Success(tr.first, tr.rest)
    else Failure("no domain", tr.rest)
  }

  def all: Parser[Token] = local ~> domain
}
