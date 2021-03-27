name := "EmailValidator4Scala"

version := "0.4.0"

scalaVersion := "2.13.5"

organization := "com.egulias"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
