package emailvalidator.parser

import scala.util.parsing.combinator._
;

class EmailParser extends RegexParsers {

//  def at: Parser[Tokens] = """([a-zA-Z_]+[46]?)|([0-9]+)|(\r\n)|(::)|(\\s+?)|(.)|(\p{Cc}+)""".r ^^ {Token.fromString(_)}

  def atom = """[a-zA-Z_]+""".r
  def escapes = "".r
  def basicLocalPart: Parser[String] = atom
  def localPart: Parser[String] = basicLocalPart ~> "@".r
  def tld: Parser[String] = atom
  def noTldDomainPart: Parser[String] = tld ~> rep("""\.""".r ~> atom) ^^ { _.toString } | tld
//  def basicDomainPart: Parser[String] = localPart ~> atom ~> rep(tld).toString()
  def emailParser: Parser[String] = localPart ~> noTldDomainPart
}

object EmailValidator extends EmailParser {
  def isValid(input: String): Boolean = {
    try {
      parseAll(emailParser, input) match {
        case Success(result, _) => true
        case failure: NoSuccess =>
          scala.sys.error(failure.msg)
          true
        case error: Error =>
          scala.sys.error(error.msg)
          false
      }
    } catch {
      case e: RuntimeException => false
    }
  }
}

//object Test extends App {
//  val result = EmailValidator.parseAll(EmailValidator.emailParser, "â@iana.org")
//  val t = Tokenizer.tokenize("example@exampel.com")
//
//  t.foreach(println)
//}
