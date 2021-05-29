package emailvalidator.lexer

import org.scalatest.funsuite.AnyFunSuite

class TokenizerSpec extends AnyFunSuite {
  test("should produce special tokens from a special string") {
    val expected = Token.special
    for (t <- expected) assert(List(t._2) == Tokenizer.tokenize(t._1))
  }

  test("should produce generic tokens for non special") {
     assert(List(GENERIC("test")) == Tokenizer.tokenize("test"))
  }

  test("should accept UTF8") {
     assert(List(GENERIC("ñü", false)) == Tokenizer.tokenize("ñü"))
  }

  test("should generate multiple types of tokens for a complex string") {
    val complexString = "A string with: Multiple\\ [t@kens]"
    val expected = List(
      GENERIC("A"),
      SPACE,
      GENERIC("string"),
      SPACE,
      GENERIC("with"),
      COLON,
      SPACE,
      GENERIC("Multiple"),
      BACKSLASH,
      SPACE,
      OPENBRACKET,
      GENERIC("t"),
      AT,
      GENERIC("kens"),
      CLOSEBRACKET
    )
    assert(expected == Tokenizer.tokenize(complexString))
  }

  test ("it has invalid tokens") {
    val value = "€" 
    // ['examp║le@symfony.com'],
    assert(true == Tokenizer.tokenize(value).contains(INVALID))
  }

}