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
        """"@""" ->
          """[1.1] failure: failed at DQUOTE(")
            |
            |"@
            |^""".stripMargin,
        """"\"@""" ->
          """[1.1] failure: failed at DQUOTE(")
            |
            |"\"@
            |^""".stripMargin,
        """"test""test"@""" ->
          """[1.1] failure: `AT(@)' expected but DQUOTE(") found
            |
            |"test"@
            |^""".stripMargin,
        """"test"."test"@""" ->
          """[1.1] failure: `AT(@)' expected but DOT(.) found
            |
            |."test"@
            |^""".stripMargin,
        """"test".test@""" ->
          """[1.1] failure: `AT(@)' expected but DOT(.) found
            |
            |.test@
            |^""".stripMargin,
        s""""test"${String.valueOf(Character.toChars(0))}@iana.org""" ->
          """[1.1] failure: `AT(@)' expected but NUL( ) found
            |
            | @
            |^""".stripMargin,
        """"test\"@""" ->
          """[1.1] failure: failed at DQUOTE(")
            |
            |"test\"@
            |^""".stripMargin,
        "\r\ntest@" ->
          s"""[1.1] failure: failed at CRLF(${"\r\n"})
              |
              |${"\r\n"}test@iana.org
              |^""".stripMargin,
        "\r\n test@" ->
          s"""[1.1] failure: failed at CRLF(${"\r\n"})
              |
              |${"\r\n"} test@iana.org
              |^""".stripMargin,
        """test;123@""" ->
          """[1.1] failure: `AT(@)' expected but SEMICOLON(;) found
            |
            |;123@
            |^""".stripMargin,
        """exam(A)ple@""" ->
          """[1.1] failure: `AT(@)' expected but SEMICOLON(;) found
            |
            |;123@
            |^""".stripMargin

      )

      for (t <- invalidLocalParts) f(t._1, t._2)
    }
    it("should be valid local parts") {
      val validLocalParts = List("â@", "example@", "example_underscore@", "example((comment))@",
        "example+@", "example+1a@", "инфо@", "müller@", "example!@", "example#@", "example$@",
      "example%@", "example&@", "example'@", "example*@", "example+@", "example-@", "example/@",
      "example=@", "example?@", "example^@", "example_@", "example`@", "example{@", "example|@",
      "example}@", "example~@")

      for (t <- validLocalParts) {
        val result = EmailParser.local(new TokenReader(t))
        assert(result.successful, s"for part $t")
      }
    }

    it("should validate correctly comments") {
      //always before @
      val validLocalParts = List("example((comment))@", "exam(s) @"
      )

      for (t <- validLocalParts) {
        val result = EmailParser.local(new TokenReader(t))
        assert(result.successful, s"for part $t")
      }
    }

    it("should validate correctly inside dquote") {
      val validLocalParts = List(
        """"example"@""",
        """"user,name"@""",
        """"user name"@""",
        """"user@name"@""",
        """"user\"name"@""",
        """"user\aname"@""",
        """"user\ name"@""",
        """""@""",
        """"\""@""")

      for (t <- validLocalParts) {
        val result = EmailParser.local(new TokenReader(t))
        assert(result.successful, s"for part $t")
      }
    }

    it("is a local part too long") {
      val email =
        "too_long_localpart_too_long_localpart_too_long_localpart_too_long_localpart@"
      val result = EmailParser.local(new TokenReader(email))
      assert(!result.successful, s"for part $email")
    }
  }
}
