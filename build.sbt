name := "EmailValidator4Scala"

version := "0.4.0"

scalaVersion := "2.11.8"

organization := "com.egulias"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

bintrayPackageLabels := Seq("email", "validation", "email validation")

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
