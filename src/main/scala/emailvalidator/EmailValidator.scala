package emailvalidator

import emailvalidator.lexer.{Token, TokenReader}
import emailvalidator.parser.EmailParser

import scala.util.parsing.input.Reader

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
  def validate(email:String): Either[Failure,Success] = result(new TokenReader(email))
  def validate(tokenReader: Reader[Token]): Either[Failure,Success] = result(tokenReader)

  private def result(tokenReader: Reader[Token]): Either[Failure, Success] = {
    val parsingResult = EmailParser.parse(tokenReader)
    if (parsingResult.successful) Right(Success())
    else Left(Failure(parsingResult.toString))
  }

}
