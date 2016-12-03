package emailvalidator.lexer

import scala.collection.immutable.HashMap
import scala.util.parsing.input.{Position, Reader}

object Tokenizer {
  val special:Map[String, Token] = HashMap[String, Token](
    ("@",AT()),
    ("(",OPENPARENTHESIS()),
    (")",CLOSEPARENTHESIS()),
    ("<",LOWERTHAN()),
    (">",GREATERTHAN()),
    ("[",OPENBRACKET()),
    ("]",CLOSEBRACKET()),
    (":",COLON()),
    (";",SEMICOLON()),
    ("\\",BACKSLASH()),
    ("/",SLASH()),
    (",",COMMA()),
    (".",DOT()),
    ("\"",DQUOTE()),
    ("'",QUOTE()),
    ("-",DASH()),
    ("::",DOUBLECOLON()),
    (" ",SPACE()),
    ("\t",HTAB()),
    ("\r",CR()),
    ("\n",LF()),
    ("\r\n",CRLF()),
    ("IPv6",IPV6TAG()),
    ("{",OPENCURLYBRACES()),
    ("}",CLOSECURLYBRACES()),
    ("\0",NUL())
  )

  def tokenize(value: String): List[Token] = {
    val r = """(?ui)([a-zA-Z_+0-9\p{L}]+)|(\r\n)|(::)|(\\s+?)|(.)|(\p{Cc}+)""".r.findAllIn(value)
//    val r = """([a-zA-Z_+]+[46]?)|([0-9]+)|(\r\n)|(::)|(\\s+?)|(.)|(\p{Cc}+)""".r.findAllIn(value)
    val l = for(
      s <- r
    ) yield stringToToken(s)
    l.toList
  }

  def stringToToken(s: String): Token = {
    special.getOrElse(s, GENERIC(s, """^[\x20-\x7F]+$""".r.findAllIn(s).nonEmpty))
  }
}

class TokenReader(override val source: String, override val offset: Int = 0) extends Reader[Token] {
  val tokenizedSource: List[Token] = Tokenizer.tokenize(source)

  override def first: Token = if(tokenizedSource.isEmpty)NUL() else tokenizedSource.head

  override def atEnd: Boolean = tokenizedSource.isEmpty

  override def pos: Position = new Position {
    override def column: Int = 1

    override def line: Int = 1

    override protected def lineContents: String = source.toString
  }

  override def rest = if(atEnd)this else new TokenReader(tokenizedSource.tail.map(_.value).mkString, offset+1)

  override def toString = s"starting at ${first.toString}"
}
