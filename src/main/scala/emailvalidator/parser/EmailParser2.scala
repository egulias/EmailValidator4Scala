package emailvalidator.parser

import emailvalidator.lexer._

import scala.util.parsing.combinator.Parsers

object EmailParser2 extends Parsers {
  type Elem = Token

  implicit def fromCondition(f: (TokenReader) => ParseResult[Token]): Parser[Token] = new Parser[Token] {
    override def apply(in: EmailParser2.Input): EmailParser2.ParseResult[Token] = body(in, f)
  }

//  implicit def fromToken(token: Token): Parser[Token] = new Parser[Token] {
//    override def apply(in: EmailParser2.Input): EmailParser2.ParseResult[Token] = {
//      body(in, tr => {
//        if (tr.first.equals(token)) Success(in.first, in.rest)
//        else Failure(s"failed at ${in.first}", in)
//      })
//
//    }
//  }

  def body(in: Input, f: (TokenReader) => ParseResult[Token]) = {
    in match {
      case in: TokenReader => f(in)
      case _ => Error("no reader", in.rest)
    }
  }

  def local: Parser[Token] = (log(dquote)("dquoute") | log(rep(atom ~> DOT()))("repeat atom") ~> atom ~> log(comment)("comment")) <~ log(AT())("finding @")

  def domain = rep1(atom)

  def comment: Parser[Token] =
    opt(OPENPARENTHESIS()) into(op => op match {
      case Some(x) => rep(log(atom)("comment atom")) ~> CLOSEPARENTHESIS()
      case None => Parser{ in => Success(null, in) }
    })

  def dquote: Parser[Token] = log(DQUOTE())("open quote") ~> opt(rep(log(atom)("double quote atom"))) ~> log(DQUOTE())("closing quote")

  def atom = acceptIf {
    case _: GENERIC => true
    case _ => false
  }(t => s"failed at $t")

  def hasAt: Parser[Token] = (tr: TokenReader) => {
    if (tr.realSource.contains(AT())) Success(tr.first, tr.rest)
    else Failure("no domain", tr.rest)
  }
}
