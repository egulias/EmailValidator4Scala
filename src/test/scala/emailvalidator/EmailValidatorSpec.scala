package emailvalidator

import org.scalatest.FunSpec

class EmailValidatorSpec extends FunSpec {
  describe("Validates emails") {
    it("should return success for a valid email") {

      assert(EmailValidator.validate("example@example").isRight)
    }

    it("should return failure for an invalid email") {
      val emails = List(
            "@example.co.uk",
            "example@",
            "example@example-.co.uk",
            "example@example-",
            "example@@example.co.uk",
            "example..example@example.co.uk",
            "example@example..co.uk",
            "<example_example>@example.fr",
            ".example@localhost",
            "example@.localhost",
            "example@localhost.",
            "example.@example.co.uk",
            "(example@localhost",
            """"example@localhost""",
            """"exa"mple@localhost""",
            "(example@localhost",
            "comment)example@localhost",
            "example(comment))@localhost",
            "example@comment)localhost",
            "example@localhost(comment))",
            "example@(comment))example.com",
            "exampl\ne@example.co.uk",
            "example@[[]",
            "exampl\te@example.co.uk",
            "example@exa\rmple.co.uk",
            "example@[\r]",
            "exam\rple@example.co.uk")

      for {
        email <- emails
      } yield assert(EmailValidator.validate(email).isLeft, email)
    }
  }
}
