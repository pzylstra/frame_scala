name := "ffm"

description := "Forest fire module"

organization := "cermb"

version := "1.0"

scalaVersion := "2.11.7"

// General dependencies
//
libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
  "org.scalautils" %% "scalautils" % "2.1.5",
  "com.vividsolutions" % "jts" % "1.13",
  "org.apache.commons" % "commons-math3" % "3.3",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test"
)


