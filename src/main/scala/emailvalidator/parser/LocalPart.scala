package emailvalidator.parser

import emailvalidator.Failure
import emailvalidator.Success
import emailvalidator.lexer._

object LocalPart {

    val invalidTokens: List[Token] = COMMA :: OPENBRACKET :: CLOSEBRACKET :: GREATERTHAN :: LOWERTHAN :: COLON :: SEMICOLON :: Nil

    def parse (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        def parserAccumulator (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
            tokens match {
                case token :: rest => token match {
                    case token: AT.type => Right(Success())
                    case token: SPACE.type => Left(Failure(s"Found [${SPACE}] ATEXT expected"))
                    case token: DOT.type if !previous.isDefined => Left(Failure(s"Found [${DOT}] at start"))
                    case token: DOT.type if previous.getOrElse(NUL).isInstanceOf[DOT.type] => Left(Failure(s"Found [${DOT}] ATEXT expected"))
                    case token: DOT.type if rest.head.isInstanceOf[AT.type] => Left(Failure(s"Found [$DOT] near [$AT]"))
                    case token: OPENPARENTHESIS.type => parseComments(token, rest, previous)
                    case _ if invalidTokens.contains(token) => Left(Failure(s"Found [${token}] ATEXT expected"))
                    case token: BACKSLASH.type => rest.head match {
                        case GENERIC(_,_) => Left(Failure(s"Escaping ATOM"))
                        case SPACE | HTAB => Left(Failure(s"Scaping ${SPACE}"))
                        case _ => Right(Success())
                        
                    }
                    case _ => parserAccumulator(rest, Option(token))
                }
                case Nil => Right(Success())
            }
        }
        parserAccumulator(tokens, None)
    }

    private def parseComments(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        Left(Failure("Unclosed parethesis, found [(]"))

    }
  
}
