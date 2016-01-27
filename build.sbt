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
  "com.typesafe.slick" %% "slick" % "3.1.1",
  "org.xerial" % "sqlite-jdbc" % "3.7.2",         // SQLite JDBC driver
  "org.slf4j" % "slf4j-nop" % "1.6.4", 
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test"
)


// Tell sbteclipse plugin to generate download source artifacts and
// create Eclipse source attachments for dependencies
EclipseKeys.withSource := true

