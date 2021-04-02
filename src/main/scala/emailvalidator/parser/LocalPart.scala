package emailvalidator.parser

import emailvalidator.Failure
import emailvalidator.Success
import emailvalidator.lexer.Token
import emailvalidator.lexer.AT

object LocalPart {
    def parse (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        def parserAccumulator (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
            tokens match {
                case token :: rest => token match {
                    case token: AT.type => Right(Success())
                    case _ => Right(Success())
                }
                case Nil => Right(Success())
            }
        }
        parserAccumulator(tokens, None)
    }
  
}
