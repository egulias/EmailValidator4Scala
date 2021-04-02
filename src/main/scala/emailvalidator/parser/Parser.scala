package emailvalidator.parser

import emailvalidator.lexer.Token
import emailvalidator.Success
import emailvalidator.Failure
import emailvalidator.lexer.INVALID

/**
  *  def sum2(ints: List[Int]): Int = {
    @tailrec
    def sumAccumulator(ints: List[Int], accum: Int): Int = {
      ints match {
        case Nil => accum
        case x :: tail => sumAccumulator(tail, accum + x)
      }
    }
    sumAccumulator(ints, 0)
  } 
  */
object Parser {
    def parse (tokens: List[Token]): Either[Failure, Success] = {
        if (tokens.contains(INVALID)) Left(Failure("Invalid Tokens"))
        else LocalPart.parse(tokens, None)
    }
}
