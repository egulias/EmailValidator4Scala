package emailvalidator.parser

import emailvalidator.lexer.TokenReader
import org.scalatest.FunSpec

class EmailParserSpec extends FunSpec {
  describe("Parses an email") {
    it("should parse the email successfully") {
      val result = EmailParser.parse(new TokenReader("example@example.com"))
      assert(result.successful)
    }

    it("should not parse the email successfully") {
      val result = EmailParser.parse(new TokenReader("example@@example.com"))
      assert(!result.successful)
    }
  }

}
