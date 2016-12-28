package emailvalidator.lexer

import scala.util.parsing.input.{OffsetPosition, Position, Reader}

object Tokenizer {

  def tokenize(value: String): List[Token] = {
    val r = """(?ui)([a-zA-Z_+0-9\p{L}]+)|(\r\n)|(\\s+?)|(.)|(\p{Cc}+)""".r.findAllIn(value)
    val l = for(
      s <- r
    ) yield Token(s)
    l.toList
  }
}

class TokenReader(override val source: String, override val offset: Int = 0) extends Reader[Token] {
  val tokenizedSource: List[Token] = Tokenizer.tokenize(source)

  override def first: Token = if(tokenizedSource.isEmpty) NUL else tokenizedSource.head

  override def atEnd: Boolean = tokenizedSource.isEmpty

  override def pos: Position = OffsetPosition(source, offset)

  override def rest = if(atEnd)this else new TokenReader(tokenizedSource.tail.map(_.value).mkString, offset+1)

  override def toString = s"starting at ${first.toString}"
}
