package emailvalidator.parser

import emailvalidator.lexer.Token
import emailvalidator.Failure
import emailvalidator.Success
import emailvalidator.lexer._

object DomainPart {

    val labelMaxLength = 63
    private def allowedToken(token: Token): Boolean = token match {
        case GENERIC(_,_) | HYPHEN | DOT => true
        case _ => false
    }

    def parse (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        def parserAccumulator (tokens: List[Token], length: Int, previous: Option[Token]): Either[Failure, Success] = {
            tokens match {
                case token :: rest => token match {
                    case AT => Left(Failure("Double AT"))
                    case OPENPARENTHESIS | CLOSEPARENTHESIS => parseComments(token, rest, previous)
                    case OPENBRACKET | CLOSEBRACKET => parseDomainLiteral(token, rest, previous)
                    case DQUOTE => Left(Failure(s"Invalid character ${DQUOTE}")) 
                    case DOT => 
                        if (previous.isEmpty) Left(Failure(s"${DOT} near ${AT}"))
                        else if (!previous.getOrElse(None).isInstanceOf[GENERIC]) Left(Failure(s"${previous.getOrElse(None)} near ${DOT}"))
                        else if (rest.size == 0) Left(Failure(s"${DOT} at the end"))
                        else parserAccumulator(rest, length + DOT.length, Option(token))
                    case l if (length > labelMaxLength) => Left(Failure("Domain part too long. More than 63 chars."))
                    case _ => if (allowedToken(token)) parserAccumulator(rest, length + token.length, Option(token)) else Left(Failure(s"Invalid character in domain ${token}"))
                }
                case Nil if !previous.getOrElse(None).isInstanceOf[GENERIC] => Left(Failure(s"${previous.getOrElse(None)} at the end"))
                case Nil => Right(Success(None))
            }
        }
        parserAccumulator(tokens, 0, None)
    }

    private def parseComments(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        def count (tokens: List[Token], counter: Int) : Either[Failure, Success] = {
            tokens match {
                case token :: rest => token match {
                    case CLOSEPARENTHESIS => count(rest, counter - 1)
                    case OPENPARENTHESIS => count(rest, counter + 1)
                    case _ => count(rest, counter)
                }
                case Nil => if (counter == 0) Right(Success(None)) else Left(Failure("Unclosed comment"))
            }
        }
        if (current == OPENPARENTHESIS) count(rest, 1)
        else  count(rest, -1)
    }

    private def parseDomainLiteral(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        def count (tokens: List[Token], counter: Int) : Either[Failure, Success] = {
            tokens match {
                case token :: rest => token match {
                    case CLOSEBRACKET => count(rest, counter - 1)
                    case OPENBRACKET => count(rest, counter + 1)
                    case GENERIC(_,_) | IPV6TAG | COLON | DOT => count(rest, counter)
                    case _ => Left(Failure(s"Invalid character in domain literal ${token}"))
                }
                //Review this error message
                case Nil => if (counter == 0) Right(Success(None)) else Left(Failure("Expecting DTEXT"))
            }
        }
        if (current == OPENBRACKET) count(rest, 1)
        else  count(rest, -1)
    }
}