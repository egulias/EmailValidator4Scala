package emailvalidator.parser

import emailvalidator.lexer.TokenReader
import org.scalatest.FunSpec

import scala.collection.immutable.HashMap
import scala.util.parsing.combinator.Parsers

class LocalPartSpec extends FunSpec {
  describe("An email local part") {
    it("should end with @") {
      val result = EmailParser2.local(new TokenReader("localpart@"))
      assert(result.successful === true)
    }
    it("fails if there is no @") {
      val result = EmailParser2.local(new TokenReader("localpart"))
      assert(result.successful === false)
    }

    it("fails for invalid local parts") {
      val f = (localPart: String, expectedMessage: String) => {
        val result = EmailParser2.local(new TokenReader(localPart))
        assert(!result.successful, s"for part $localPart")
        assert(result.toString === expectedMessage)
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
        """"test""test"@iana.org"""->
        """[1.1] failure: `AT(@)' expected but DQUOTE(") found
          |
          |"test"@iana.org
          |^""".stripMargin,
        """"test"."test"@iana.org""" ->
        """[1.1] failure: `AT(@)' expected but DOT(.) found
          |
          |."test"@iana.org
          |^""".stripMargin

      )

      for (t <- invalidLocalParts) f(t._1, t._2)
    }
  }
}
