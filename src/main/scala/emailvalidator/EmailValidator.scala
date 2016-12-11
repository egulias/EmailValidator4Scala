package emailvalidator

import emailvalidator.lexer.{Token, TokenReader}
import emailvalidator.parser.EmailParser

import scala.util.parsing.input.Reader

sealed trait ValidationResult {
  def hasWarnings: Boolean
  def warnings: List[Warning]
  def isSuccess: Boolean
  def isFailure: Boolean
}

sealed case class Warning (msg:String, explanation:String)

case class Success(warns: List[Warning] = Nil) extends ValidationResult {
  override def hasWarnings:Boolean = warns.nonEmpty

  override def isFailure = false

  override def isSuccess = true

  override def warnings: List[Warning] = warns
}


case class Failure(msg:String) extends ValidationResult {
  override def hasWarnings = false

  override def isFailure = true

  override def isSuccess = false

  override def warnings = Nil
}

object EmailValidator {
  def validate(email:String): ValidationResult = result(new TokenReader(email))
  def validate(tokenReader: Reader[Token]): ValidationResult = result(tokenReader)

  private def result(tokenReader: Reader[Token]) = {
    val parsingResult = EmailParser.parse(tokenReader)
    if (parsingResult.successful) Success()
    else Failure(parsingResult.toString)
  }

}
