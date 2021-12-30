package emailvalidator.lexer

import scala.collection.immutable.HashMap

sealed trait Token {
  def value:String
  def isAscii: Boolean = true
  def length:Int = value.length
}

object Token {
  private val haveMeaning:Map[String, Token] = HashMap[String, Token](
    ("@",AT),
    ("(",OPENPARENTHESIS),
    (")",CLOSEPARENTHESIS),
    ("<",LOWERTHAN),
    (">",GREATERTHAN),
    ("[",OPENBRACKET),
    ("]",CLOSEBRACKET),
    (":",COLON),
    (";",SEMICOLON),
    ("\\",BACKSLASH),
    ("/",SLASH),
    (",",COMMA),
    (".",DOT),
    ("\"",DQUOTE),
    ("'",QUOTE),
    ("-",HYPHEN),
    (" ",SPACE),
    ("\t",HTAB),
    ("\r",CR),
    ("\n",LF),
    ("\r\n",CRLF),
    ("IPv6",IPV6TAG),
    ("\u0000",NUL),
  )

  private val special:Map[String, Token] = HashMap[String, Token](
    ("*", SPECIAL("*")),
    ("^", SPECIAL("^")),
    ("`", SPECIAL("`")),
    ("~", SPECIAL("~")),
    ("!", SPECIAL("!")),
    ("&", SPECIAL("&")),
    ("^", SPECIAL("^")),
    ("%", SPECIAL("%")),
    ("$", SPECIAL("$")),
    ("`", SPECIAL("`")),
    ("|", SPECIAL("|")),
    ("~", SPECIAL("~")),
    ("{", SPECIAL("{")),
    ("}", SPECIAL("}")),
    ("=", SPECIAL("=")),
    ("+", SPECIAL("+")),
    ("_", SPECIAL("_")),
    ("¡", SPECIAL("¡")),
    ("?", SPECIAL("?")),
    ("#", SPECIAL("#")),
    ("¨", SPECIAL("¨")),

  )   
  def apply(value: String): Token = {
    val withMeaning: PartialFunction[String, Token] = {
      case x if(haveMeaning.contains(x)) => haveMeaning.get(x).get
    }

    val isSpecial: PartialFunction[String, Token] = {
      case x if(special.contains(x)) => special.get(x).get
    }

    val isGeneric: PartialFunction[String, Token] = {
      case x if("""(?ui)[\p{S}\p{C}\p{Cc}]+""".r.findAllIn(x).isEmpty) => GENERIC (x, """^[\x20-\x7F]+$""".r.findAllIn(x).nonEmpty)
    }

    val isInvalid: PartialFunction[String, Token] ={ case x => INVALID}

    (withMeaning orElse isSpecial orElse isGeneric orElse isInvalid)(value)
  }
}

sealed case class GENERIC(value:String, override val isAscii: Boolean = true) extends Token {
  def canBeIPv4:Boolean = "^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$".r.findAllIn(value).nonEmpty
  def canBeIPv6:Boolean ="^[0-9A-Fa-f]{1,4}$".r.findAllIn(value).nonEmpty
}

sealed case class IPv4(value:String) extends Token
sealed case class IPv6(value:String) extends Token

sealed case class SPECIAL(value:String) extends Token

//missing code and name
case object AT extends Token {def value: String = "@"}
case object DOT extends Token { def value:String = "."}
case object OPENPARENTHESIS extends Token { def value:String = "("}
case object CLOSEPARENTHESIS extends Token { def value:String = ")"}
case object LOWERTHAN extends Token { def value:String = "<"}
case object GREATERTHAN extends Token { def value:String = ">"}
case object OPENBRACKET extends Token { def value:String = "["}
case object CLOSEBRACKET extends Token { def value:String = "]"}
case object COLON extends Token { def value:String = ":"}
case object SEMICOLON extends Token { def value:String = ";"}
case object BACKSLASH extends Token { val value:String = "\\"}
case object SLASH extends Token { def value:String = "/"}
case object COMMA extends Token { def value:String = ","}
case object DQUOTE extends Token { def value:String = "\""}
case object QUOTE extends Token { def value:String = "'"}
case object HYPHEN extends Token { def value:String = "-"}
case object DOUBLECOLON extends Token { def value:String = "::"}
case object SPACE extends Token { def value:String = " "}
case object HTAB extends Token { def value:String = "\t"}
case object CR extends Token { def value:String = "\r"}
case object LF extends Token { def value:String = "\n"}
case object CRLF extends Token { def value:String = "\r\n"}
case object IPV6TAG extends Token { def value:String = "IPv6"}
case object OPENCURLYBRACES extends Token { def value:String = "{"}
case object CLOSECURLYBRACES extends Token { def value:String = "}"}
case object NUL extends Token { def value:String = "\u0000"}
case object INVALID extends Token { def value:String = "Invalid Token"}