package emailvalidator.lexer

import org.scalatest.FunSpec

class TokenizerSpec extends FunSpec{
  describe ("A Tokenizer") {
    it("should produce special tokens from a special string") {
      val expected = Token.special
      for (t <- expected) assert(List(t._2) === Tokenizer.tokenize(t._1))
    }

    it("should produce generic tokens for non special") {
       assert(List(GENERIC("test")) === Tokenizer.tokenize("test"))
    }

    it("should generate multiple types of tokens for a complex string") {
      val complexString = "A string with: Multiple\\ [t@kens]"
      val expected = List(
        GENERIC("A"),
        SPACE(),
        GENERIC("string"),
        SPACE(),
        GENERIC("with"),
        COLON(),
        SPACE(),
        GENERIC("Multiple"),
        BACKSLASH(),
        SPACE(),
        OPENBRACKET(),
        GENERIC("t"),
        AT(),
        GENERIC("kens"),
        CLOSEBRACKET()
      )
      assert(expected === Tokenizer.tokenize(complexString))
    }

//    it("should generate IPv4 tokens") {
//      val ip = "192.168.1.2"
//      val expected = IPv4("192") :: DOT() :: IPv4("168") :: DOT() :: IPv4("1") :: DOT() :: IPv4("2") :: Nil
//      assert(expected === Tokenizer.tokenize(ip))
//    }
//
//    it("should generate IPv6 tokens") {
//      val ip = "[IPv6:12ab:ba12:1234:faab]"
//      val expected = OPENBRACKET() :: IPV6TAG() :: COLON() :: IPv6("12ab") :: COLON() :: IPv6("ba12") :: COLON() :: IPv6("1234") :: COLON() :: IPv6("faab") :: CLOSEBRACKET() :: Nil
//      assert(expected === Tokenizer.tokenize(ip))
//    }
//
//    it("should generate IPv6 tokens for 1 digit") {
//      val ip = "[IPv6:12ab:ba12:1234:1]"
//      val expected = OPENBRACKET() :: IPV6TAG() :: COLON() :: IPv6("12ab") :: COLON() :: IPv6("ba12") :: COLON() :: IPv6("1234") :: COLON() :: IPv6("1") :: CLOSEBRACKET() :: Nil
//      assert(expected === Tokenizer.tokenize(ip))
//    }
  }

}
