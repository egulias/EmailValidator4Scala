package emailvalidator.parser

import emailvalidator.lexer.TokenReader
import org.scalatest.FunSpec

import scala.collection.immutable.HashMap

class DomainPartSpec extends FunSpec {
  describe("An email domain part") {
    def emailAssert = (part: String, expectedMessage: String) => {
      val result = EmailParser.domain(new TokenReader(part))
      assert(!result.successful, s"for part $part")
//      assert(result.toString === expectedMessage)
    }
    it("should be invalid") {
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
//        """â.org""" ->
//          """[1.1] failure: failed at GENERIC(â,false)
//            |
//            |â.org
//            |^""".stripMargin,
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
      for (t <- invalidDomainParts) emailAssert(t._1, t._2)
    }

    it("should be a valid domain") {
      val validDomainParts = List("localhost", "example(example).com", "письмо.рф", "möller.com",
        "email*", "email!", "email&", "email^", "email%", "email$", "with-hyphen.com", "профи-инвест.рф"
      )
      for (domainString <- validDomainParts) {
        val result = EmailParser.domain(new TokenReader(domainString))
        assert(result.successful, s"for part $domainString")
      }

    }

    it("should be a valid IPv6 literal address") {
      val validDomainLiterals = List(
        "[IPv6:12ab:bc45::1]", "[IPv6:12ab:bc45:5bfa:14ff]",
        "[IPv6:12ab:bc45:5bfa:14ff:1234]", "[IPv6:12ab:bc45:5bfa:14ff:2345:3456]",
        "[IPv6:12ab:bc45:5bfa:14ff:2345:3456:4567]", "[IPv6:12ab:bc45:5bfa:14ff:2345:3456:4567:5678]",
        "[IPv6::12ab:bc45:5bfa:14ff:2345:3456:4567::]"
      )
      for (domainString <- validDomainLiterals) {
        val result = EmailParser.domain(new TokenReader(domainString))
        assert(result.successful, s"for part $domainString because $result")
      }
    }

    it("should be an invalid IPv6 literal address") {
      val invalidDomainLiterals = List("[IPv6:12ab:gc45::1]", "[IPv6:12ab:ac45:1234:1:]",
        "[IPv6::ddab:dd45:5bfa:14ff:2345:3456:4567:abcd::]",
        "[IPv6:12ab:bc45:5bfa:14ff:2345:3456:4567:5678:6789]",
        "[IPv6::ddab:dd45:5bfa:14ff:2345:3456:4567]"
      )

      for (domainString <- invalidDomainLiterals) {
        val result = EmailParser.domain(new TokenReader(domainString))
        assert(!result.successful, s"for part $domainString")
      }
    }

    it("should be a valid IPv4 literal address") {
      val validDomainLiterals = List("[192.168.1.2]")
      for (domainString <- validDomainLiterals) {
        val result = EmailParser.domain(new TokenReader(domainString))
        assert(result.successful, s"for part $domainString")
      }
    }

    it("should be an invalid IPv4 literal address") {
      val invalidDomainLiterals = List("[256.168.2.1]", "[192.256.2.1]",
        "[192.168.256.1]", "[192.168.2.256]"
      )
      for (domainString <- invalidDomainLiterals) {
        val result = EmailParser.domain(new TokenReader(domainString))
        assert(!result.successful, s"for part $domainString")
      }
    }

  }
}
