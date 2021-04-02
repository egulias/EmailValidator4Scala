package emailvalidator.parser

import org.scalatest.funsuite.AnyFunSuite
import emailvalidator.lexer.INVALID
import emailvalidator.Success
import emailvalidator.Failure

class ParserSpec extends AnyFunSuite {
    test("InvalidCharsNotAllowed") {
        assert(Left(Failure("Invalid Tokens")) == EmailParser.parse(INVALID :: Nil))
    }
}
