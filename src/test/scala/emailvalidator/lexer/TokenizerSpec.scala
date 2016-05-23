package emailvalidator.lexer

import org.scalatest.FunSpec

class TokenizerSpec extends FunSpec{
  describe ("A Tokenizer") {
    it("should produce special tokens from a special string") {
      val expected = Tokenizer.special
      for (t <- expected) assert(List(t._2) === Tokenizer.tokenize(t._1))
    }

    it("should produce generic tokens for non special") {
       assert(List(Generic("test")) === Tokenizer.tokenize("test"))
    }

    it("should generate multiple types of tokens for a complex string") {
      val complexString = "A string with: Multiple\\ [t@kens]"
      val expected = List(
        Generic("A"),
        SPACE(),
        Generic("string"),
        SPACE(),
        Generic("with"),
        COLON(),
        SPACE(),
        Generic("Multiple"),
        BACKSLASH(),
        SPACE(),
        OPENBRACKET(),
        Generic("t"),
        AT(),
        Generic("kens"),
        CLOSEBRACKET()
      )
      assert(expected === Tokenizer.tokenize(complexString))
    }
  }

}
