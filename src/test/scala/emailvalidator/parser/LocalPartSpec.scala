package emailvalidator.parser

import emailvalidator.lexer.TokenReader
import org.scalatest.FunSpec

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
      val f = (t: String) => {
          val result = EmailParser2.local(new TokenReader(t))
          !result.successful //&& result.toString == "f"
        }
      val invalidEmails = ".dotAtStart@" :: Nil

      for (t <- invalidEmails) assert(f(t))
    }
  }
}
