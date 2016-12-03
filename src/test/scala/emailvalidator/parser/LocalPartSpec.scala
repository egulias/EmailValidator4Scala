package emailvalidator.parser

import emailvalidator.lexer.TokenReader
import org.scalatest.FunSpec

import scala.collection.immutable.HashMap

class LocalPartSpec extends FunSpec {
  describe("An email local part") {
    it("should end with @") {
      val result = EmailParser.local(new TokenReader("localpart@"))
      assert(result.successful === true)
    }
    it("fails if there is no @") {
      val result = EmailParser.local(new TokenReader("localpart"))
      assert(result.successful === false)
    }

    it("fails for invalid local parts") {
      val f = (localPart: String, expectedMessage: String) => {
        val result = EmailParser.local(new TokenReader(localPart))
        assert(!result.successful, s"for part $localPart")
        //        assert(result.toString === expectedMessage)
      }

      val invalidLocalParts = HashMap("unclosed(comment@" ->
        """[1.1] failure: `CLOSEPARENTHESIS())' expected but AT(@) found
          |
          |@
          |^""".stripMargin,
        ".dotAtStart@" ->
          """[1.1] failure: failed at DOT(.)
            |
            |.dotAtStart@
            |^""".stripMargin,
        "example..other@" ->
          """[1.1] failure: failed at DOT(.)
            |
            |.other@
            |^""".stripMargin,
        "user  name@example.com" ->
          """[1.1] failure: `AT(@)' expected but SPACE( ) found
            |
            |  name@example.com
            |^""".stripMargin,
        "example.@example.com" ->
          """[1.1] failure: failed at AT(@)
            |
            |@example.com
            |^""".stripMargin,
        "example(example]example@example.com" ->
          """[1.1] failure: `CLOSEPARENTHESIS())' expected but CLOSEBRACKET(]) found
            |
            |]example@example.com
            |^""".stripMargin,
        "exa\\mple@example.com" ->
          """[1.1] failure: `AT(@)' expected but BACKSLASH(\) found
            |
            |\mple@example.com
            |^""".stripMargin,
        "usern,ame@example.com" ->
          """[1.1] failure: `AT(@)' expected but COMMA(,) found
            |
            |,ame@example.com
            |^""".stripMargin,
        """"@iana.org""" ->
          """[1.1] failure: failed at DQUOTE(")
            |
            |"@iana.org
            |^""".stripMargin,
        """"\"@iana.org""" ->
          """[1.1] failure: failed at DQUOTE(")
            |
            |"\"@iana.org
            |^""".stripMargin,
        """"test""test"@iana.org""" ->
          """[1.1] failure: `AT(@)' expected but DQUOTE(") found
            |
            |"test"@iana.org
            |^""".stripMargin,
        """"test"."test"@iana.org""" ->
          """[1.1] failure: `AT(@)' expected but DOT(.) found
            |
            |."test"@iana.org
            |^""".stripMargin,
        """"test".test@iana.org""" ->
          """[1.1] failure: `AT(@)' expected but DOT(.) found
            |
            |.test@iana.org
            |^""".stripMargin,
        s""""test"${String.valueOf(Character.toChars(0))}@iana.org""" ->
          """[1.1] failure: `AT(@)' expected but NUL( ) found
            |
            | @iana.org
            |^""".stripMargin,
        """"test\"@iana.org""" ->
          """[1.1] failure: failed at DQUOTE(")
            |
            |"test\"@iana.org
            |^""".stripMargin,
        "\r\ntest@iana.org" ->
          s"""[1.1] failure: failed at CRLF(${"\r\n"})
              |
              |${"\r\n"}test@iana.org
              |^""".stripMargin,
        "\r\n test@iana.org" ->
          s"""[1.1] failure: failed at CRLF(${"\r\n"})
              |
              |${"\r\n"} test@iana.org
              |^""".stripMargin,
        """test;123@example.com""" ->
          """[1.1] failure: `AT(@)' expected but SEMICOLON(;) found
            |
            |;123@example.com
            |^""".stripMargin

      )

      for (t <- invalidLocalParts) f(t._1, t._2)
    }
    it("should be valid local parts") {
      /*
            ['fabien_potencier@example.fr'],
            ['example@localhost'],
            ['fab\'ien@symfony.com'],
            ['fab\ ien@symfony.com'],
            ['example((example))@fakedfake.co.uk'],
            ['example@faked(fake).co.uk'],
            ['fabien+@symfony.com'],
            ['инфо@письмо.рф'],
            ['"username"@example.com'],
            ['"user,name"@example.com'],
            ['"user name"@example.com'],
            ['"user@name"@example.com'],
            ['"user\"name"@example.com'],
            ['"\a"@iana.org'],
            ['"test\ test"@iana.org'],
            ['""@iana.org'],
            ['"\""@iana.org'],
            ['müller@möller.de'],
            ['test@email*'],
            ['test@email!'],
            ['test@email&'],
            ['test@email^'],
            ['test@email%'],
            ['test@email$'],
            ["1500111@профи-инвест.рф"],
       */
      val validLocalParts = List("â@", "example@", "example_underscore@",
        """exam\'ple@""",
        """exam\ ple@""",
        """example((comment))@""", "example+@", "example+1a@", "инфо@",
        """"example"@""",
        """"user,name"@""",
        """"user name"@""",
        """"user@name"@""",
        """"user\"name"@""",
        """"user\aname"@""",
        """"user\ name"@""",
        """""@""",
        """"\""@""",
        """müller@""")

      for (t <- validLocalParts) {
        val result = EmailParser.local(new TokenReader(t))
        assert(result.successful, s"for part $t")
      }
    }
    it("should validate correctly escaped chars") {
      assert(false)
    }
    it("should validate correctly comments") {
      assert(false)
    }
    it("should validate correctly inside dquote") {
      assert(false)
    }
  }
}
