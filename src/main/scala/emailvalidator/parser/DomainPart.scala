package emailvalidator.parser

import emailvalidator.lexer.Token
import emailvalidator.Failure
import emailvalidator.Success
import emailvalidator.lexer._

object DomainPart {
  
    def parse (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        def parserAccumulator (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
            tokens match {
                case token :: rest => token match {
                    case AT => Left(Failure("Double AT"))
                    case OPENPARENTHESIS | CLOSEPARENTHESIS => parseComments(token, rest, previous)
                    case OPENBRACKET => parseDomainLiteral(token, rest, previous)
                    case DQUOTE => Left(Failure(s"Invalid character ${DQUOTE}")) 
                    case DOT => 
                        if (previous.isEmpty) Left(Failure(s"${DOT} near ${AT}"))
                        else if (!previous.getOrElse(None).isInstanceOf[GENERIC]) Left(Failure(s"${previous.getOrElse(None)} near ${DOT}"))
                        else if (rest.size == 0) Left(Failure(s"${DOT} at the end"))
                        else parserAccumulator(rest, Option(token))
                    case _ => parserAccumulator(rest, Option(token))
                }
                case Nil if !previous.getOrElse(None).isInstanceOf[GENERIC] => Left(Failure(s"${previous.getOrElse(None)} at the end"))
                case Nil => Right(Success(None))
            }
        }
        parserAccumulator(tokens, None)
    }

    private def parseComments(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        def countP (tokens: List[Token], counter: Int) : Either[Failure, Success] = {
            tokens match {
                case token :: rest => token match {
                    case CLOSEPARENTHESIS => countP(rest, counter - 1)
                    case OPENPARENTHESIS => countP(rest, counter + 1)
                    case _ => countP(rest, counter)
                }
                
                case Nil => if (counter == 0) Right(Success(None)) else Left(Failure("Unclosed comment"))
            }
        }
        if (current == OPENPARENTHESIS) countP(rest, 1)
        else  countP(rest, -1)
    }

    private def parseDomainLiteral(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        Right(Success())
    }
}