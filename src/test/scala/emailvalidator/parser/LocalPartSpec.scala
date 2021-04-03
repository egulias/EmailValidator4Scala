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
        val invalidLocalParts = List[(List[Token], String)](
            (GENERIC("local") :: SPACE :: GENERIC("part") :: AT :: Nil, s"Found [${SPACE.toString()}] ATEXT expected"),
            (GENERIC("local") :: DOT :: DOT :: GENERIC("part") :: AT :: Nil, s"Found [${DOT.toString}] ATEXT expected")
        )

        //
        for {
            local <- invalidLocalParts
        } yield assert(Left(Failure(local._2)) == LocalPart.parse(local._1, None), local)
    }
}
