package emailvalidator.parser

import emailvalidator.lexer.Token
import emailvalidator.Success
import emailvalidator.Failure
import emailvalidator.lexer.INVALID

object Parser {
    def parse (tokens: List[Token]): Either[Failure, Success] = {
        if (tokens.contains(INVALID)) Left(Failure("Invalid Tokens"))
        else LocalPart.parse(tokens, None)
    }
}
