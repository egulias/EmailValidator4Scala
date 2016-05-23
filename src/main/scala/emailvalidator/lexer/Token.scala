package emailvalidator.lexer

sealed trait Token {
  def value:String
}

sealed case class Generic(override val value:String) extends Token
sealed case class Other (override val value: String) extends Token
sealed case class AT (override val value:String = "@") extends Token
sealed case class DOT (override val value:String = ".") extends Token
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
