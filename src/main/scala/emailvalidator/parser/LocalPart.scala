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
                    case AT => Right(Success())
                    case SPACE => Left(Failure(s"Found [${SPACE}] ATEXT expected"))
                    case DOT if !previous.isDefined => Left(Failure(s"Found [${DOT}] at start"))
                    case DOT if previous.getOrElse(NUL).isInstanceOf[DOT.type] => Left(Failure(s"Found [${DOT}] ATEXT expected"))
                    case DOT if rest.head.isInstanceOf[AT.type] => Left(Failure(s"Found [$DOT] near [$AT]"))
                    case OPENPARENTHESIS => parseComments(token, rest, previous)
                    case DQUOTE => parseQuotedString(token, rest, previous)
                    case _ if invalidTokens.contains(token) => Left(Failure(s"Found [${token}] ATEXT expected"))
                    case token: BACKSLASH.type => rest.head match {
                        case GENERIC(_,_) => Left(Failure(s"ATEXT found after FWS"))
                        case SPACE | HTAB => Left(Failure(s"Scaping ${SPACE}"))
                        case _ => parserAccumulator(rest, previous)
                        
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
  
    private def parseQuotedString(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        rest match {
            case  qsToken :: following => qsToken match {
                case DQUOTE => following.head match {
                    case AT => Right(Success())
                    case _ => Left(Failure("Unescapaed double quote, found [\"]"))
                }
                case BACKSLASH => parseQuotedString(following.drop(1).head, following.drop(1), Option(following.head))
                case _ => parseQuotedString(following.head, following, Option(qsToken))
            }
            case Nil => Left(Failure("Unclosed quoted string"))
        }
    }
}
