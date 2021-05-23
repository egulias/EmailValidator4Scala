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
        /**
            ['\r\ntest@iana.org'],
            ['\r\n test@iana.org'],
            ['\r\n \r\ntest@iana.org'],
            ['\r\n \r\ntest@iana.org'],
            ['\r\n \r\n test@iana.org'],
            ['test;123@foobar.com'],
            ['examp║le@symfony.com'],
            ['0'],
            [0],
          */
        val invalidLocalParts = List[(List[Token], String)](
            (LF :: Nil, "Empty FWS"),
            (DQUOTE :: GENERIC("local") :: BACKSLASH :: DQUOTE :: AT :: Nil, "Missing closing DQUOTE. Quotes string should be a unit"),
            (DQUOTE :: GENERIC("local") :: DQUOTE :: GENERIC("\u0000") :: AT :: Nil, s"ATEXT found, ${AT} expected"),
            (DQUOTE :: GENERIC("local") :: DQUOTE :: DOT :: DQUOTE :: GENERIC("local") :: DQUOTE:: AT :: Nil,
                "Unclosed quoted string"),
            (DQUOTE :: GENERIC("local") :: DQUOTE :: DQUOTE :: GENERIC("local") :: DQUOTE:: AT :: Nil,
                s"Unescaped double quote, expected ${BACKSLASH}"),
            (GENERIC("test") :: OPENPARENTHESIS :: GENERIC("test") :: CLOSEBRACKET :: GENERIC("test") :: AT :: Nil,
                s"Unclosed parethesis, found [(]"),
            (DQUOTE :: GENERIC("local") :: DQUOTE :: GENERIC("test") :: AT :: Nil, s"ATEXT found, ${AT} expected"),
            (DQUOTE :: BACKSLASH :: DQUOTE :: AT :: Nil, "Missing closing DQUOTE. Quotes string should be a unit"),
            (DQUOTE :: DQUOTE :: DQUOTE :: AT :: Nil, s"Unescaped double quote, expected ${BACKSLASH}"),
            (GENERIC("local") :: OPENBRACKET :: GENERIC("part") :: CLOSEBRACKET :: AT :: Nil, s"Found [${OPENBRACKET}] ATEXT expected"),
            (GENERIC("local") :: COMMA :: GENERIC("part") :: AT :: Nil, s"Found [${COMMA}] ATEXT expected"),
            (GENERIC("local") :: SPACE :: GENERIC("part") :: AT :: Nil, s"Found [${SPACE}] ATEXT expected"),
            (GENERIC("local") :: DOT :: DOT :: GENERIC("part") :: AT :: Nil, s"Found [${DOT}] ATEXT expected"),
            (GENERIC("localpart") :: DOT :: AT :: Nil, s"Found [${DOT}] near [${AT}]"),
            (OPENPARENTHESIS :: GENERIC("localpart") :: AT :: Nil, s"Unclosed parethesis, found [(]"),
            (DOT :: GENERIC("localpart") :: AT :: Nil, s"Found [${DOT}] at start"),
            (GENERIC("local") :: BACKSLASH :: GENERIC("part") :: AT :: Nil, s"ATEXT found after FWS"),
        )
        for {
            local <- invalidLocalParts
        } yield assert(Left(Failure(local._2)) == LocalPart.parse(local._1, None), local)
    }
}
