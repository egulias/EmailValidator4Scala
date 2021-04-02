package emailvalidator.parser

import org.scalatest.funsuite.AnyFunSuite
import emailvalidator.Success
import emailvalidator.Failure
import emailvalidator.lexer.AT
import emailvalidator.lexer.GENERIC
import emailvalidator.lexer.SPACE

class LocalPartSpec extends AnyFunSuite {
    test("parse a valid local part") {
        assert(Right(Success()) == LocalPart.parse(GENERIC("localpart") :: AT :: Nil, None))
    }

    test("parse an invalid local part") {
        assert(Left(Failure(s"Found [${SPACE.toString()}] ATEXT expected")) == LocalPart.parse(GENERIC("local") :: SPACE :: GENERIC("part") :: AT :: Nil, None))
    }
}
