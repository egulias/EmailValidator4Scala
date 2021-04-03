package emailvalidator.parser

import emailvalidator.Failure
import emailvalidator.Success
import emailvalidator.lexer._

object LocalPart {
    def parse (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        def parserAccumulator (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
            tokens match {
                case token :: rest => token match {
                    case token: AT.type => Right(Success())
                    case token: SPACE.type => Left(Failure(s"Found [${SPACE}] ATEXT expected"))
                    case token: DOT.type if previous.getOrElse(NUL).isInstanceOf[DOT.type] => Left(Failure(s"Found [${DOT}] ATEXT expected"))
                    case _ => parserAccumulator(rest, Option(token))
                }
                case Nil => Right(Success())
            }
        }
        parserAccumulator(tokens, None)
    }
  
}
