package emailvalidator.parser

import emailvalidator.lexer
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

  def local = (log(dquote)("dquoute") | log(rep(atom ~> opt(DOT() ~> atom )))("repeat atom") <~ (log(escaped)("escaped") | log(comment)("comment2")) | Parser{in=>Success(in.first, in.rest)}) <~ log(AT())("finding @")

  def atom = acceptIf {
    case _: GENERIC => true
    case _ => false
  }(t => s"failed at $t")


  def comment: Parser[Token] = OPENPARENTHESIS() ~> (atom | comment) <~ CLOSEPARENTHESIS()

  def dquote = log(DQUOTE())("open quote") ~> rep(log(dquoteAtom)("double quote atom")) <~ log(DQUOTE())("closing quote")

  def dquoteAtom:Parser[Token] = atom | COMMA() | AT() | SPACE() | (BACKSLASH() ~> DQUOTE()) |(BACKSLASH() <~ dquoteAtom) | BACKSLASH()

  def escaped = log(BACKSLASH())("escaped backslash") ~ log(consumeTokenNot(atom))("not atom") ~> atom

  private def consumeTokenNot[T](p: => Parser[T]): Parser[Unit] = Parser { in =>
    p(in) match {
      case Success(_, _) => Failure("Expected failure", in)
      case _ => Success(in.first, in.rest)
    }
  }

  def domain = phrase(log(literalDomain)("litdom") | (log(domainAtom)("domain atom") ~> opt(comment) <~ log(rep(DOT() ~> domainAtom))("repeat domain atom")))

  def literalDomain = OPENBRACKET() ~> (log(IPv6)("IPv6") | log(IPv4)("IPv4")) <~ CLOSEBRACKET()

  def IPv6 = IPV6TAG() ~ COLON() ~ repsep(opt(hex), COLON())

  def hex = atom

  def IPv4 = log(repsep(IPpart, DOT()))("repsep") into {
    case Nil => Parser[Token] {in => Success(in.first, in)}
    case xs if xs.length > 4 => Parser[Token] {in => Failure("More than 4 elements in IPv4", in)}
    case xs => Parser[Token] {in => Success(in.first, in)}
  }

  def IPpart = atom


  def domainAtom = acceptIf {
    case c: GENERIC => true
    case _ => false
  }(t => s"failed at $t")

  def hasAt: Parser[Token] = (tr: TokenReader) => {
    if (tr.tokenizedSource.contains(AT())) Success(tr.first, tr.rest)
    else Failure("no domain", tr.rest)
  }
}
