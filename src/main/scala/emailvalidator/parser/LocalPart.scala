package emailvalidator.parser

import emailvalidator.Failure
import emailvalidator.Success
import emailvalidator.lexer._

object LocalPart {

    val invalidTokens: List[Token] = COMMA :: OPENBRACKET :: CLOSEBRACKET :: GREATERTHAN :: LOWERTHAN :: COLON :: SEMICOLON :: INVALID :: Nil

    def parse (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        def parserAccumulator (tokens: List[Token], previous: Option[Token]): Either[Failure, Success] = {
            tokens match {
                case token :: rest => token match {
                    case AT => Right(Success())
                    case SPACE => {
                        if(previous.getOrElse(NUL) != BACKSLASH) Left(Failure(s"Found [${SPACE}] ATEXT expected"))
                        else parserAccumulator(rest, Option(token))
                    }
                    case DOT if !previous.isDefined => Left(Failure(s"Found [${DOT}] at start"))
                    case DOT if previous.getOrElse(NUL).isInstanceOf[DOT.type] => Left(Failure(s"Found [${DOT}] ATEXT expected"))
                    case DOT if rest.head.isInstanceOf[AT.type] => Left(Failure(s"Found [$DOT] near [$AT]"))
                    case OPENPARENTHESIS => parseComments(token, rest, Option(token))
                    case DQUOTE => parseQuotedString(token, rest, Option(token))
                    case BACKSLASH => rest.head match {
                        case GENERIC(_,_) => Left(Failure(s"ATEXT found after FWS"))
                        case _ => parserAccumulator(rest, Option(token))
                    }
                    case SPACE | HTAB | CR | LF | CRLF => parseFWS(token, rest, Option(token))
                    case _ if invalidTokens.contains(token) => Left(Failure(s"Found [${token}] ATEXT expected"))
                    case _ => parserAccumulator(rest, Option(token))
                }
                case Nil => Right(Success())
            }
        }
        parserAccumulator(tokens, None)
    }

    private def parseFWS(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {

        val isEscaped =  if(previous.getOrElse(NUL) == BACKSLASH && !current.isInstanceOf[GENERIC]) true else false

        
        //is FWS?
        val isFWS = current match {
            case SPACE | HTAB | CR | LF | CRLF => true
            case _ => false
        }
        //CRLF in FWS check
        if (current == CRLF) Right(Success())
        else if (rest.head match {
            case SPACE | HTAB => false
            case _ => true
        }) Left(Failure("CRLFX2 | CRLFAtTheEnd")) // return new InvalidEmail(new CRLFX2(), $this->lexer->token['value']);
        else Right(Success())
        // end check

        val isValidFWS = current match {
            case CR => false //return new InvalidEmail(new CRNoLF(), $this->lexer->token['value']);
            case LF | NUL => false // return new InvalidEmail(new ExpectingCTEXT(), $this->lexer->token['value']);
            case _ => true

        }
        val isNearAT = rest.head.isInstanceOf[GENERIC] && previous.getOrElse(NUL) != AT // return new InvalidEmail(new AtextAfterCFWS(), $this->lexer->token['value']);


        //if ($this->lexer->isNextToken(EmailLexer::S_AT) || $previous['type']  === EmailLexer::S_AT) {
        //    $this->warnings[CFWSNearAt::CODE] = new CFWSNearAt();
        //} else {
        //    $this->warnings[CFWSWithFWS::CODE] = new CFWSWithFWS();
        //}

        if (isFWS && isValidFWS && isNearAT) Right(Success())
        else Left(Failure("aaaaa"))

    }

    private def parseComments(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        Left(Failure("Unclosed parethesis, found [(]"))
    }
  
    private def parseQuotedString(current: Token, rest: List[Token], previous: Option[Token]): Either[Failure, Success] = {
        rest match {
            case  qsToken :: following => qsToken match {
                case DQUOTE => following.head match {
                    case AT => Right(Success())
                    case GENERIC(_,_) if (following.drop(1).head == AT ) => Left(Failure(s"ATEXT found, ${AT} expected")) 
                    case DQUOTE if !previous.getOrElse(None).isInstanceOf[BACKSLASH.type] =>  Left(Failure(s"Unescaped double quote, expected ${BACKSLASH}")) 
                    case DQUOTE => parseQuotedString(qsToken, following, previous)
                    case _ => Left(Failure("Unclosed quoted string")) //review error message
                }
                case BACKSLASH => parseQuotedString(following.head, following.drop(1), Option(following.head))
                case Nil => Left(Failure("Missing closing DQUOTE. Quotes string should be a unit"))
                case _ => if (following.isEmpty) Left(Failure("Missing closing DQUOTE. Quotes string should be a unit"))
                    else  parseQuotedString(following.head, following, Option(qsToken))
            }
            case Nil => Left(Failure("Unclosed quoted string"))
        }
    }
}