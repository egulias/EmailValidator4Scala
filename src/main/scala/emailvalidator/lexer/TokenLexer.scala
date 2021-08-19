package emailvalidator.lexer

object Tokenizer {

  def tokenize(value: String): List[Token] = {
    val r = """(?ui)([a-zA-Z_+0-9\p{L}]+)|(\r\n)|(\\s+?)|(.)|(\p{Cc}+)""".r.findAllIn(value)
    val l = for(
      s <- r
    ) yield Token(s)
    l.toList
  }
}

class TokenLexer (val source: String, val offset: Int = 0) {

  val tokenizedSource: List[Token] = Tokenizer.tokenize(source)
  val pos = offset
  private val maxPos = tokenizedSource.length

  def first: Token = if(tokenizedSource.isEmpty) NUL else tokenizedSource.head

  def next: Token = if (pos + 1 > tokenizedSource.length) NUL else tokenizedSource(pos + 1)

  def atEnd: Boolean = tokenizedSource.isEmpty

  def rest = if(atEnd)this else new TokenLexer(tokenizedSource.tail.map(_.value).mkString, offset+1)

  override def toString = s"starting at ${first.toString}"
}