package emailvalidator

import emailvalidator.lexer.{Token, TokenLexer}
import emailvalidator.lexer.Tokenizer

sealed trait ValidationResult {
  def warnings: Option[List[Warning]] = None
  def isSuccess: Boolean
  def isFailure: Boolean
}

sealed case class Warning (msg:String, explanation:String)

case class Success(override val warnings: Option[List[Warning]] = None) extends ValidationResult {
  override def isFailure = false
  override def isSuccess = true
}


case class Failure(msg:String) extends ValidationResult {
  override def isFailure = true
  override def isSuccess = false
}

object EmailValidator {
  def validate(email:Option[String]): Either[Failure,Success] = {
    val tokens: List[Token] = Tokenizer.tokenize(email.getOrElse(""))
    Left(Failure("test"))
  }
}
