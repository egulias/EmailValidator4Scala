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
                    case OPENPARENTHESIS => parseComments(token, rest, previous)
                    case OPENBRACKET => parseDomainLiteral(token, rest, previous)
                    case DOT if !previous.getOrElse(None).isInstanceOf[GENERIC] => Left(Failure(s"${previous.getOrElse(None)} near {$DOT}"))
                    case _ => parserAccumulator(rest, Option(token))
                }
                case Nil if !previous.getOrElse(None).isInstanceOf[GENERIC] => Left(Failure(s"${previous.getOrElse(None)} at the end"))
                case Nil => Right(Success(None))
            }
        }
        parserAccumulator(tokens, None)
    }

    private def parseComments(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        Right(Success())
    }

    private def parseDomainLiteral(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        Right(Success())
    }
}