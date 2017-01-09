package emailvalidator.parser

import emailvalidator.lexer._

import scala.annotation.tailrec
import scala.collection.immutable.Nil
import scala.util.parsing.combinator.Parsers

object EmailParser extends Parsers {
  type Elem = Token

  def local:Parser[Token] =
    localPartSize ~ (dquote | rep1(atom ~> opt(DOT <~ atom )) <~ (comment | Parser{in=>Success(in.first, in)})) ~> AT

  private def localPartSize = Parser {in =>
    val sizeWithoutAt:Int = in.asInstanceOf[TokenReader].tokenizedSource.filter(t => !t.eq(AT)).map(t=>t.length).sum
    if (sizeWithoutAt < 65) Success(in.first,in)
    else Failure("More than 64 chars in local part", in)
  }

  private def atom = acceptIf {
    case _: GENERIC => true
    case QUOTE | DASH | SLASH => true
    case _ => false
  }(t => s"failed at $t")

  private def comment: Parser[Token] = OPENPARENTHESIS ~> (atom | comment) <~ CLOSEPARENTHESIS ~ opt(SPACE)

  private def dquote = DQUOTE ~> commit(rep(dquoteAtom) <~ DQUOTE)

  private def dquoteAtom:Parser[Token] = atom | COMMA | AT | SPACE | (BACKSLASH ~> DQUOTE) | BACKSLASH

  def domain:Parser[Object] = phrase(domainPartSize ~ (literalDomain | (domainUnit ~ rep(DOT ~> domainUnit))))
  def domainUnit = rep1(domainAtom ~ opt(DASH~domainAtom))

  private def domainPartSize = Parser {in =>
    val sizeWithoutAt:Int = in.asInstanceOf[TokenReader].tokenizedSource.filter(t => !t.eq(AT)).map(t=>t.length).sum
    if (sizeWithoutAt < 255) Success(in.first,in)
    else Failure("More than 254 chars in local part", in)
  }

  private def literalDomain = OPENBRACKET ~> (IPv6 | IPv4) <~ CLOSEBRACKET

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

  private def IPv4 = repsep(IPv4part, DOT) into {
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
    case _ => false
  }(t => s"failed at $t")

  def parse:Parser[~[Token, Object]] = local ~ domain
}
