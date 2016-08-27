package emailvalidator.parser

import emailvalidator.lexer
import emailvalidator.lexer._

import scala.util.parsing.combinator.Parsers

object EmailParser2 extends Parsers {
  type Elem = Token

  def apply(in: String) = all(new TokenReader(in))

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

  def t =
    repsep(log(atom)("local part atom"), log(opt(DOT()))("dot separator")).flatMap(l=>{
    if(l.isEmpty)failure(s"Expected ATOM")
    else success(l)
  })

  def pass = Parser{ in => Success(in.first, in) }

  def local: Parser[Token] = log(rep(atom ~> DOT()))("repeat atom") ~> atom ~> log(comment)("comment") <~ log(AT())("finding @")

  def comment: Parser[Token] =
    opt(OPENPARENTHESIS()) into(op => op match {
      case Some(x) => rep(log(atom)("comment atom")) ~> CLOSEPARENTHESIS()
      case None => op.get

    })

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
