package emailvalidator.parser

import org.scalatest.funsuite.AnyFunSuite
import emailvalidator.Success
import emailvalidator.Failure
import emailvalidator.lexer._

class LocalPartSpec extends AnyFunSuite {
   test("parse a valid local part")  {
       // MISSING FOLDING WHITE SPACE AND PARENTHESIS
// local part too long test missing
/**
  *         return array(

  */
        val validLocalParts = List[List[Token]](
            GENERIC("localpart") :: AT :: Nil,
            GENERIC("инфо") :: AT :: Nil,
            GENERIC("müller") :: AT :: Nil,
            GENERIC("â") :: AT :: Nil,
            GENERIC("1500111") :: AT :: Nil,
            GENERIC("local") :: SPECIAL("_") :: GENERIC("part"):: AT :: Nil, 
            GENERIC("local") ::  SPECIAL("+") :: GENERIC("part") :: AT :: Nil,
            GENERIC("local") :: BACKSLASH :: QUOTE :: GENERIC("part") :: AT :: Nil,
            GENERIC("local") :: SPECIAL("=") :: GENERIC("part") :: AT :: Nil,
            
        )
        for {
            local <- validLocalParts
        } yield assert(Right(Success()) == LocalPart.parse(local, None), local)
    }

    test("parse valid FWS") {
        val validLocalParts = List[List[Token]](
            GENERIC("test") :: BACKSLASH :: SPACE :: Nil,
            GENERIC("local") :: BACKSLASH :: SPACE :: GENERIC("part") :: AT :: Nil,
        )
        for {
            local <- validLocalParts
        } yield assert(Right(Success()) == LocalPart.parse(local, None), local)
    }

    test("parse valid quoted string") {
        val validDoulbeQuotedLocalParts = List[List[Token]](
            DQUOTE :: GENERIC("localpart") :: DQUOTE :: AT :: Nil,
            DQUOTE :: GENERIC("local") :: COMMA :: GENERIC("part") :: DQUOTE :: AT :: Nil,
            DQUOTE :: GENERIC("local") :: SPACE :: GENERIC("part") :: DQUOTE :: AT :: Nil,
            DQUOTE :: GENERIC("local") :: BACKSLASH :: SPACE :: GENERIC("part") :: DQUOTE :: AT :: Nil,
            DQUOTE :: GENERIC("local") :: AT :: GENERIC("part") :: DQUOTE :: AT :: Nil,
            DQUOTE :: GENERIC("local") :: BACKSLASH :: DQUOTE :: GENERIC("part") :: DQUOTE :: AT :: Nil,
            DQUOTE :: BACKSLASH :: GENERIC("localpart") :: DQUOTE :: AT :: Nil,
            DQUOTE :: DQUOTE :: AT :: Nil,
            DQUOTE :: BACKSLASH :: DQUOTE :: DQUOTE :: AT :: Nil,
        )
        for {
            local <- validDoulbeQuotedLocalParts 
        } yield assert(Right(Success()) == LocalPart.parse(local, None), local)
    }
    test("parse INvalid quoted string")(pending)

    test("parse valid comment ()") {

            //['example((example))@fakedfake.co.uk'],
        val validLocalPartsWithComments = List[List[Token]](
            GENERIC("local") :: OPENPARENTHESIS :: GENERIC("comment") :: CLOSEPARENTHESIS :: AT :: Nil,
            GENERIC("local") :: OPENPARENTHESIS :: OPENPARENTHESIS :: GENERIC("comment") :: CLOSEPARENTHESIS :: CLOSEPARENTHESIS :: AT :: Nil,
        )
        for {
            local <- validLocalPartsWithComments 
        } yield assert(Right(Success()) == LocalPart.parse(local, None), local)
    }

    test("parse an invalid local part") {
        //invalid CFWS
        /*
                    [new InvalidEmail(new AtextAfterCFWS(), "\n"), "exampl\ne@example.co.uk"],
            [new InvalidEmail(new AtextAfterCFWS(), "\t"), "exampl\te@example.co.uk"],
            [new InvalidEmail(new CRNoLF(), "\r"), "exam\rple@example.co.uk"],
            (CRLF :: AT :: Nil, "Empty FWS"),
            (LF :: AT :: Nil, "Empty FWS"),
            CRLF :: SPACE :: CRLF :: GENERIC("test") :: AT :: Nil,
            CRLF :: SPACE :: CRLF :: SPACE :: GENERIC("test") :: AT :: Nil,
            CRLF :: SPACE :: GENERIC("test") :: AT :: Nil,
            [chr(226) . '@iana.org'],
            ['\r\ntest@iana.org'],
            ['\r\n \r\ntest@iana.org'],
            ['\r\n \r\n test@iana.org'],
            ['test;123@foobar.com'],
            ['examp║le@symfony.com'],
            ['0'],
            [0],
        */
        val invalidLocalParts = List[(List[Token], String)](
            (GENERIC("test") :: INVALID :: GENERIC("test") :: AT :: Nil, s"Found [${INVALID}] ATEXT expected"),
            (GENERIC("test") :: SEMICOLON :: GENERIC("123") :: AT :: Nil, s"Found [${SEMICOLON}] ATEXT expected"),
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
            (OPENPARENTHESIS :: GENERIC("localpart") :: CLOSEPARENTHESIS :: GENERIC("more") :: AT :: Nil, s"Unclosed parethesis, found [(]"),
            (DOT :: GENERIC("localpart") :: AT :: Nil, s"Found [${DOT}] at start"),
            (GENERIC("local") :: BACKSLASH :: GENERIC("part") :: AT :: Nil, s"ATEXT found after FWS"),
        )
        for {
            local <- invalidLocalParts
        } yield assert(Left(Failure(local._2)) == LocalPart.parse(local._1, None), local)
    }
}
