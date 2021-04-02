package emailvalidator

import org.scalatest.funsuite.AnyFunSuite

class EmailValidatorSpec extends AnyFunSuite {
  test("Validates emails") {
      assert(EmailValidator.validate(Option("example@example")).isRight)
  }
  test("should return failure for an invalid email") {
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
    } yield assert(EmailValidator.validate(Option(email)).isLeft, email)
  }
}
