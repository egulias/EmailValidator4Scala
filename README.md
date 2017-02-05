# EmailValidator4Scala

[![Build Status](https://travis-ci.org/egulias/EmailValidator4Scala.svg?branch=master)](https://travis-ci.org/egulias/EmailValidator4Scala)
[![Download](https://api.bintray.com/packages/egulias/maven/emailvalidator4scala/images/download.svg) ](https://bintray.com/egulias/maven/emailvalidator4scala/_latestVersion)

Simple RFC compliant EmailValidator for Scala.

**Suported RFCs**

[5321](http://www.rfcreader.com/#rfc5321), [5322](http://www.rfcreader.com/#rfc5322), [6530](http://www.rfcreader.com/#rfc6530), [6531](http://www.rfcreader.com/#rfc6531), [6532](http://www.rfcreader.com/#rfc6532).

Install it!
-----------
EmailValidator4Scala is available on [JCenter]!!!
In your `build.sbt` file first add JCenter resolver

```scala
resolvers ++= Seq(
    "Jcenter" at "https://jcenter.bintray.com/"
)
```

Then add the dependency

```scala
libraryDependencies ++= Seq(
    "com.egulias" % "emailvalidator4scala_2.11" % "0.4.0"
)
```

[JCenter]: https://bintray.com/egulias/maven/emailvalidator4scala

Getting Started
---------------
EmailValidator for Scala is an RFC compliant email validator. Will return a `Right` or `Left` result with information about the error.

```scala
def validate(email:String): Either[Failure,Success]
```
([source](https://github.com/egulias/EmailValidator4Scala/blob/master/src/main/scala/emailvalidator/EmailValidator.scala#L28))

### A valid email

```scala
scala> import emailvalidator.EmailValidator
import emailvalidator.EmailValidator
scala> print (EmailValidator.validate("test@example.com"))
Right(Success(None))
```

### An invalid email
```scala
scala> import emailvalidator.EmailValidator
import emailvalidator.EmailValidator
scala>   print (EmailValidator.validate("test(a)a@example.com"))
Left(Failure([1.5] failure: `AT' expected but GENERIC(a,true) found

a@example.com
    ^))
```

Licence
-----------
Released under the MIT License attached with this code in the LICENSE file.


