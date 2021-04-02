package emailvalidator.parser

import org.scalatest.funsuite.AnyFunSuite
import emailvalidator.Success
import emailvalidator.lexer.AT
import emailvalidator.lexer.GENERIC

class LocalPartSpec extends AnyFunSuite {
    test("parse a valid local part") {
        assert(Right(Success()) == LocalPart.parse(GENERIC("localpart") :: AT :: Nil, None))
    }
}
