package emailvalidator

import org.scalatest.FunSpec

class EmailValidatorSpec extends FunSpec {
  describe("Validates emails") {
    it("should return success for a valid email") {
      assert(EmailValidator.validate("example@example").isSuccess)
    }

    it("should return failure for an invalid email") {
      assert(EmailValidator.validate("example@@example").isFailure)
    }
  }

}
