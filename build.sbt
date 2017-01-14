name := "EmailValidator4Scala"

version := "0.4"

scalaVersion := "2.11.8"

organization := "egulias"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
