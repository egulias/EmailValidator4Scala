package emailvalidator.parser

import emailvalidator.lexer.TokenReader
import org.scalatest.FunSpec

import scala.collection.immutable.HashMap

class DomainPartSpec extends FunSpec {
  describe("An email domain part") {
    it("should be invalid") {
      val f = (localPart: String, expectedMessage: String) => {
        val result = EmailParser.domain(new TokenReader(localPart))
        assert(!result.successful, s"for part $localPart")
        assert(result.toString === expectedMessage)
      }
      val invalidDomainParts = HashMap("example.com test" ->
        """[1.1] failure: `DOT(.)' expected but SPACE( ) found
          |
          | test
          |^""".stripMargin,
        """localhost\""" ->
          """[1.1] failure: `DOT(.)' expected but BACKSLASH(\) found
            |
            |\
            |^""".stripMargin,
        """localhost.""" ->
          """[1.1] failure: end of input
            |
            |
            |^""".stripMargin,
        """ example . com""" ->
          """[1.1] failure: failed at SPACE( )
            |
            | example . com
            |^""".stripMargin,
        """(fake.com""" ->
          """[1.1] failure: failed at OPENPARENTHESIS(()
            |
            |(fake.com
            |^""".stripMargin,
        """fake].com""" ->
          """[1.1] failure: `DOT(.)' expected but CLOSEBRACKET(]) found
            |
            |].com
            |^""".stripMargin,
        """example,com""" ->
          """[1.1] failure: `DOT(.)' expected but COMMA(,) found
            |
            |,com
            |^""".stripMargin,
        """â.org""" ->
          """[1.1] failure: failed at GENERIC(â,false)
            |
            |â.org
            |^""".stripMargin,
        """iana.org \r\n""" ->
          """[1.1] failure: `DOT(.)' expected but SPACE( ) found
            |
            | \r\n
            |^""".stripMargin,
       """iana/icann.org"""  ->
        """[1.1] failure: `DOT(.)' expected but SLASH(/) found
          |
          |/icann.org
          |^""".stripMargin,
        """foo;bar.com""" ->
        """[1.1] failure: `DOT(.)' expected but SEMICOLON(;) found
          |
          |;bar.com
          |^""".stripMargin,
        """example..com""" ->
        """[1.1] failure: failed at DOT(.)
          |
          |.com
          |^""".stripMargin,
        """email."""" ->
        """[1.1] failure: failed at DQUOTE(")
          |
          |"
          |^""".stripMargin,
        """email>""" ->
        """[1.1] failure: `DOT(.)' expected but GREATERTHAN(>) found
          |
          |>
          |^""".stripMargin

      )
      for (t <- invalidDomainParts) f(t._1, t._2)
    }
  }
}
