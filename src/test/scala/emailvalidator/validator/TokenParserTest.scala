package emailvalidator.validator

//import emailvalidator.lexer.EmailValidator
//import org.scalatest.FlatSpec
//
//class TokenParserTest extends FlatSpec {
//  "These " should " be valid emails" in {
//    val valid: List[String] = List(
//      "example@example", "example@example.com", "example@example.co.uk",
//     "exam_ple@example.com", "fab\\'ien@symfony.com", "fab\\ ien@symfony.com")
//    for (email <- valid) {
//      assert(EmailValidator.isValid(email), "for email " + email)
//    }
//  }


  /**
    *
  array('â@iana.org'),
            array('fabien@symfony.com'),
            array('example@example.co.uk'),
            array('fabien_potencier@example.fr'),
            array('example@localhost'),
            array('fab\'ien@symfony.com'),
            array('fab\ ien@symfony.com'),
            array('example((example))@fakedfake.co.uk'),
            array('example@faked(fake).co.uk'),
            array('fabien+@symfony.com'),
            array('инфо@письмо.рф'),
            array('"username"@example.com'),
            array('"user,name"@example.com'),
            array('"user name"@example.com'),
            array('"user@name"@example.com'),
            array('"\a"@iana.org'),
            array('"test\ test"@iana.org'),
            array('""@iana.org'),
            array('"\""@iana.org'),
            array('müller@möller.de'),
            array('test@email*'),
            array('test@email!'),
            array('test@email&'),
            array('test@email^'),
            array('test@email%'),
            array('test@email$'),
    */
//}
