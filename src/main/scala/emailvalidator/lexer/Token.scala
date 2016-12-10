package emailvalidator.lexer

import scala.collection.immutable.HashMap

sealed trait Token {
  def value:String
  def isAscii: Boolean = true
}

object Token {
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
    (" ",SPACE()),
    ("\t",HTAB()),
    ("\r",CR()),
    ("\n",LF()),
    ("\r\n",CRLF()),
    ("IPv6",IPV6TAG()),
    ("\0",NUL())
  )

  def apply(value: String): Token =
    special.getOrElse(value, GENERIC(value, """^[\x20-\x7F]+$""".r.findAllIn(value).nonEmpty))

}

sealed case class GENERIC(value:String, override val isAscii: Boolean = true) extends Token {
  def canBeIPv4:Boolean = "^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$".r.findAllIn(value).nonEmpty
  def canBeIPv6:Boolean ="^[0-9A-Fa-f]{1,4}$".r.findAllIn(value).nonEmpty
}
sealed case class OTHER(override val value: String) extends Token
sealed case class AT () extends Token {
  override val value: String = "@"
}
//object DOT extends Token {
//  override val value: String = "."
//}
sealed case class DOT(override val value:String = ".") extends Token
sealed case class OPENPARENTHESIS(override val value:String = "(") extends Token
sealed case class CLOSEPARENTHESIS(override val value:String = ")") extends Token
sealed case class LOWERTHAN(override val value:String = "<") extends Token
sealed case class GREATERTHAN(override val value:String = ">") extends Token
sealed case class OPENBRACKET(override val value:String = "[") extends Token
sealed case class CLOSEBRACKET(override val value:String = "]") extends Token
sealed case class COLON(override val value:String = ":") extends Token
sealed case class SEMICOLON(override val value:String = ";") extends Token
sealed case class BACKSLASH(override val value:String = "\\") extends Token
sealed case class SLASH(override val value:String = "/") extends Token
sealed case class COMMA(override val value:String = ",") extends Token
sealed case class DQUOTE(override val value:String = "\"") extends Token
sealed case class QUOTE(override val value:String = "'") extends Token
sealed case class DASH(override val value:String = "-") extends Token
sealed case class DOUBLECOLON(override val value:String = "::") extends Token
sealed case class SPACE(override val value:String = " ") extends Token
sealed case class HTAB(override val value:String = "\t") extends Token
sealed case class CR(override val value:String = "\r") extends Token
sealed case class LF(override val value:String = "\n") extends Token
sealed case class CRLF(override val value:String = "\r\n") extends Token
sealed case class IPV6TAG(override val value:String = "IPv6") extends Token
sealed case class OPENCURLYBRACES(override val value:String = "{") extends Token
sealed case class CLOSECURLYBRACES(override val value:String = "}") extends Token
sealed case class NUL(override val value:String = "\0") extends Token

sealed case class IPv4(value:String) extends Token
sealed case class IPv6(value:String) extends Token
