package emailvalidator.parser

import emailvalidator.lexer._

import scala.annotation.tailrec
import scala.collection.immutable.Nil
import scala.util.parsing.combinator.Parsers

object EmailParser extends Parsers {
  type Elem = Token

  def local = (log(dquote)("dquoute") | log(rep(atom ~> opt(DOT <~ atom )))("repeat atom") <~ (log(comment)("comment2") | Parser{in=>Success(in.first, in)})) ~> log(AT)("finding @")

  private def atom = acceptIf {
    case _: GENERIC => true
    case QUOTE | DASH | SLASH => true
    case _ => false
  }(t => s"failed at $t")

  private def comment: Parser[Token] = OPENPARENTHESIS ~> (atom | comment) <~ CLOSEPARENTHESIS ~ opt(SPACE)

  private def dquote = log(DQUOTE)("open quote") ~> commit(rep(log(dquoteAtom)("double quote atom")) <~ log(DQUOTE)("closing quote"))

  private def dquoteAtom:Parser[Token] = atom | COMMA | AT | SPACE | (BACKSLASH ~> DQUOTE) | BACKSLASH

  def domain = phrase(log(literalDomain)("litdom") | (log(rep1(domainAtom))("domain atom") ~ log(rep(DOT ~> domainAtom))("repeat domain atom")))

  private def literalDomain = OPENBRACKET ~> (log(IPv6)("IPv6") | log(IPv4)("IPv4")) <~ CLOSEBRACKET

  private def IPv6 = IPV6TAG ~ (COLON ~ opt(COLON)) ~ rep(opt(IPv6part) ~ COLON) into {
    case _ ~ (COLON~Some(_)) ~ xs =>
      @tailrec
      def inspect(l:List[_], acc:Int): Parser[Token] = {
        if(acc > 7) Parser[Token]{ in => Failure("More than 7 groups", in)}
        else {
          l match {
            case (None~COLON) :: Nil => Parser[Token]{ in => Success(in.first, in)}
            case _ :: ls => inspect(ls, acc+1)
            case Nil => Parser[Token] {in => Failure("Missing groups", in)}
          }
        }
      }
      inspect(xs, 0)
    case _ ~ _ ~ gs if gs.size > 7 =>
      Parser[Token]{ in => Failure("More than 8 groups", in)}
    case _ => Parser[Token]{ in => Success(in.first, in.rest)}
  }

  private def IPv6part = acceptIf {
    case x: GENERIC => x.canBeIPv6
    case _ => false
  }(t => s"failed at $t")

  private def IPv4 = log(repsep(IPv4part, DOT))("repsep") into {
    case Nil => Parser[Token] {in => Success(in.first, in)}
    case xs if xs.length > 4 => Parser[Token] {in => Failure("More than 4 elements in IPv4", in)}
    case _ => Parser[Token] {in => Success(in.first, in)}
  }

  private def IPv4part = acceptIf {
    case x: GENERIC => x.canBeIPv4
    case _ => false
  }(t => s"failed at $t")

  private def domainAtom = acceptIf {
    case _: GENERIC => true
    case DASH => true
    case _ => false
  }(t => s"failed at $t")

  def parse:Parser[~[Token, Object]] = local ~ domain
}
