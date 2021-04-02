package emailvalidator.parser

import org.scalatest.funsuite.AnyFunSuite
import emailvalidator.Success
import emailvalidator.Failure
import emailvalidator.lexer._

class LocalPartSpec extends AnyFunSuite {
    test("parse a valid local part") {
        assert(Right(Success()) == LocalPart.parse(GENERIC("localpart") :: AT :: Nil, None))
    }

    test("parse an invalid local part") {
        val invalidLocalParts = List(
            GENERIC("local") :: SPACE :: GENERIC("part") :: AT :: Nil,
            GENERIC("local") :: DOT :: DOT :: GENERIC("part") :: AT :: Nil
        )

        for {
            local <- invalidLocalParts
        } yield assert(Left(Failure(s"Found [${SPACE.toString()}] ATEXT expected")) == LocalPart.parse(local, None), local)
    }
}
