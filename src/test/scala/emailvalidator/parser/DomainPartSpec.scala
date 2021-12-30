package emailvalidator.parser

import org.scalatest.funsuite.AnyFunSuite
import emailvalidator.Success
import emailvalidator.Failure
import emailvalidator.lexer._

class DomainPartSpec extends AnyFunSuite {
    test("parse a valid domain part") {
        assert(Right(Success()) == DomainPart.parse(GENERIC("example") :: DOT :: GENERIC("com") :: Nil, None))
        assert(Right(Success()) == DomainPart.parse(GENERIC("example") :: DOT :: GENERIC("co") :: DOT :: GENERIC("uk") ::Nil, None))
        assert(Right(Success()) == DomainPart.parse(GENERIC("example") :: Nil, None))
        assert(Right(Success()) == DomainPart.parse(GENERIC("письмо", false) :: DOT :: GENERIC("рф", false) :: Nil, None))
        assert(Right(Success()) == DomainPart.parse(GENERIC("müller", false) :: DOT :: GENERIC("de") :: Nil, None))
        assert(Right(Success()) == DomainPart.parse(GENERIC("профи", false) :: HYPHEN :: GENERIC("инвест", false) :: DOT :: GENERIC("рф", false) :: Nil, None))
        assert(Right(Success()) == DomainPart.parse(GENERIC("example") :: HYPHEN :: GENERIC("domain") :: DOT :: GENERIC("com") :: Nil, None))
        assert(Right(Success()) == DomainPart.parse(GENERIC("example") :: OPENPARENTHESIS :: GENERIC("comment") :: CLOSEPARENTHESIS :: DOT :: GENERIC("com") :: Nil, None))
        assert(Right(Success()) == DomainPart.parse(OPENBRACKET :: IPV6TAG :: COLON :: GENERIC("2001") :: COLON :: GENERIC("db8a") :: COLON :: GENERIC("fdde") 
            :: COLON :: GENERIC("2001") :: COLON :: GENERIC("cba1") :: COLON :: GENERIC("dad4") :: CLOSEBRACKET :: Nil, None))
        assert(Right(Success()) == DomainPart.parse(OPENBRACKET :: GENERIC("127") :: DOT :: GENERIC("0") :: DOT :: GENERIC("0") :: DOT :: GENERIC("0") :: CLOSEBRACKET :: Nil, None))
        assert(Right(Success()) == DomainPart.parse(GENERIC("127") :: DOT :: GENERIC("0") :: DOT :: GENERIC("0") :: DOT :: GENERIC("0") :: Nil, None))
    }

    test("parse an invalid domain part") {
  /*
            ['test@' . chr(226) . '.org'],
            ['example@toolonglocalparttoolonglocalparttoolonglocalparttoolonglocalparttoolonglocalparttoolonglocal'.
            'parttoolonglocalparttoolonglocalparttoolonglocalparttoolonglocalparttoolonglocalparttoolonglocalpart'.
            'toolonglocalparttoolonglocalparttoolonglocalparttoolonglocalpar'],
            ['example@toolonglocalparttoolonglocalparttoolonglocalparttoolonglocalpart.co.uk'],
            ['example@toolonglocalparttoolonglocalparttoolonglocalparttoolonglocalpart.test.co.uk'],
            ['example@test.toolonglocalparttoolonglocalparttoolonglocalparttoolonglocalpart.co.uk'],
  */
        val invalidDomainParts = List[(List[Token], String)](
            (GENERIC("example") :: DOT :: DOT :: GENERIC("com") :: Nil, s"${DOT} near ${DOT}"),
            (AT :: GENERIC("example") :: Nil, s"Double AT"),
            (GENERIC("example") :: HYPHEN :: Nil, s"${HYPHEN} at the end"),
            (GENERIC("example") :: HYPHEN :: DOT :: GENERIC("com") :: Nil, s"${HYPHEN} near ${DOT}"),
            (DOT :: GENERIC("example") :: DOT :: GENERIC("com") :: Nil, s"${DOT} near ${AT}"),
            (GENERIC("example") :: DOT :: Nil, s"${DOT} at the end"),
            (DQUOTE :: DQUOTE :: DQUOTE :: GENERIC("example") :: DOT :: GENERIC("com") :: DQUOTE :: DQUOTE :: Nil, s"Invalid character ${DQUOTE}"),
            (GENERIC("unclosed") :: CLOSEPARENTHESIS :: GENERIC("comment") :: DOT :: GENERIC ("com")  :: Nil, s"Unclosed comment"),
            (GENERIC("unclosed") :: OPENPARENTHESIS :: GENERIC("comment") :: CLOSEPARENTHESIS :: CLOSEPARENTHESIS :: DOT :: GENERIC ("com")  :: Nil, s"Unclosed comment"),
            (GENERIC("unclosed") :: OPENPARENTHESIS :: OPENPARENTHESIS:: GENERIC("comment") :: CLOSEPARENTHESIS :: DOT :: GENERIC ("com")  :: Nil, s"Unclosed comment"),
            (OPENBRACKET :: OPENBRACKET :: CLOSEBRACKET :: Nil, "Expecting DTEXT"),
            (GENERIC("exam") :: CR :: GENERIC("le") :: DOT :: GENERIC("com") :: Nil, s"Invalid character in domain ${CR}"),
            (OPENBRACKET :: CR :: CLOSEBRACKET  :: Nil, s"Invalid character in domain literal ${CR}"),
            (GENERIC("example") :: DOT :: GENERIC("com") :: SPACE :: GENERIC("more"):: Nil, s"Invalid character in domain ${SPACE}"),
            (OPENBRACKET :: GENERIC("example") :: CLOSEPARENTHESIS :: DOT :: GENERIC("com") :: Nil, s"Invalid character in domain literal ${CLOSEPARENTHESIS}"),
            (OPENPARENTHESIS :: GENERIC("example") :: CLOSEBRACKET :: DOT :: GENERIC("com") :: Nil, "Unclosed comment"),
            (GENERIC("example") :: CLOSEBRACKET :: DOT :: GENERIC("com") :: Nil, "Expecting DTEXT"),
            (GENERIC("example") :: AT :: GENERIC("example") :: DOT :: GENERIC("com") :: Nil, "Double AT"),
            (GENERIC("example") :: BACKSLASH :: GENERIC("example") :: DOT :: GENERIC("com") :: Nil, s"Invalid character in domain ${BACKSLASH}"),
            (GENERIC("example") :: BACKSLASH :: BACKSLASH :: Nil, s"Invalid character in domain ${BACKSLASH}"),
            (GENERIC("example") :: DOT :: Nil, s"${DOT} at the end"),
            (GENERIC("example") :: SPACE :: GENERIC("more"):: DOT :: GENERIC("com") :: Nil, s"Invalid character in domain ${SPACE}"),
            (GENERIC("example") :: COMMA :: GENERIC("com") :: Nil, s"Invalid character in domain ${COMMA}"),
            (GENERIC("example") :: SLASH :: GENERIC("example") :: DOT :: GENERIC("com") :: Nil, s"Invalid character in domain ${SLASH}"),
            (OPENBRACKET :: GENERIC("127") :: DOT :: BACKSLASH :: GENERIC("0") :: DOT :: GENERIC("0") :: DOT :: GENERIC("0") :: CLOSEBRACKET :: Nil, s"Invalid character in domain literal ${BACKSLASH}"),
            (GENERIC("iana") :: DOT :: GENERIC("org") :: SPACE :: Nil, s"Invalid character in domain ${SPACE}"),
            (GENERIC("foo") :: SEMICOLON :: GENERIC("bar") :: DOT :: GENERIC("com") :: Nil, s"Invalid character in domain ${SEMICOLON}"),
            (GENERIC("foo") :: SPECIAL("+") :: GENERIC("bar") :: DOT :: GENERIC("com") :: Nil, s"Invalid character in domain ${SPECIAL("+")}"),
            (GENERIC("foo") :: LOWERTHAN :: GENERIC("bar") :: DOT :: GENERIC("com") :: Nil, s"Invalid character in domain ${LOWERTHAN}"),

        )

        for {
            local <- invalidDomainParts
        } yield assert(Left(Failure(local._2)) == DomainPart.parse(local._1, None), s"expected ${local}")
    }
}
