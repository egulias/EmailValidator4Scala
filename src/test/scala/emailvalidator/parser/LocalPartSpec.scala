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
          val result  = EmailParser2.local(new TokenReader(localPart))
          assert(!result.successful, s"for part $localPart")
          assert(result.toString === expectedMessage)
        }

      val invalidLocalParts = HashMap("unclosed(comment@" ->
        """[1.1] failure: failed at AT(@)
          |
          |@
          |^""".stripMargin,
        ".dotAtStart@" ->
          """[1.1] failure: failed at DOT(.)
            |
            |.dotAtStart@
            |^""".stripMargin)

      for (t <- invalidLocalParts) f(t._1, t._2)
    }
  }
}
