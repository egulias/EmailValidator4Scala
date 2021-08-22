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
  //        "example@[\r]",
  //        "exam\rple@example.co.uk")
  /*
              ['test@example.com test'],
            ['example@example@example.co.uk'],
            ['test_exampel@example.fr]'],
            ['example@local\host'],
            ['example@localhost\\'],
            ['example@localhost.'],
            ['username@ example . com'],
            ['username@ example.com'],
            ['example@(fake].com'],
            ['example@(fake.com'],
            ['username@example,com'],
            ['test@' . chr(226) . '.org'],
            ['test@iana.org \r\n'],
            ['test@iana.org \r\n '],
            ['test@iana.org \r\n \r\n'],
            ['test@iana.org \r\n\r\n'],
            ['test@iana.org  \r\n\r\n '],
            ['test@iana/icann.org'],
            ['test@foo;bar.com'],
            ['test@example..com'],
            ["test@examp'le.com"],
            ['email.email@email."'],
            ['test@email>'],
            ['test@email<'],
            ['test@email{'],
            ['username@examp,le.com'],
            ['test@ '],
            ['invalidipv4@[127.\0.0.0]'],
            ['test@example.com []'],
            ['test@example.com. []'],
            ['test@test. example.com'],
            ['example@toolonglocalparttoolonglocalparttoolonglocalparttoolonglocalparttoolonglocalparttoolonglocal'.
            'parttoolonglocalparttoolonglocalparttoolonglocalparttoolonglocalparttoolonglocalparttoolonglocalpart'.
            'toolonglocalparttoolonglocalparttoolonglocalparttoolonglocalpar'],
            ['example@toolonglocalparttoolonglocalparttoolonglocalparttoolonglocalpart.co.uk'],
            ['example@toolonglocalparttoolonglocalparttoolonglocalparttoolonglocalpart.test.co.uk'],
            ['example@test.toolonglocalparttoolonglocalparttoolonglocalparttoolonglocalpart.co.uk'],
            ['test@email*a.com'],
            ['test@email!a.com'],
            ['test@email&a.com'],
            ['test@email^a.com'],
            ['test@email%a.com'],
            ['test@email$a.com'],
            ['test@email`a.com'],
            ['test@email|a.com'],
            ['test@email~a.com'],
            ['test@email{a.com'],
            ['test@email}a.com'],
            ['test@email=a.com'],
            ['test@email+a.com'],
            ['test@email_a.com'],
            ['test@email¡a.com'],
            ['test@email?a.com'],
            ['test@email#a.com'],
            ['test@email¨a.com'],
            ['test@email€a.com'],
            ['test@email$a.com'],
            ['test@email£a.com'],
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
            (OPENBRACKET :: CR :: CLOSEBRACKET  :: Nil, s"Invalid character in domain ${CR}")

        )

        for {
            local <- invalidDomainParts
        } yield assert(Left(Failure(local._2)) == DomainPart.parse(local._1, None), s"expected ${local}")
    }
}
