name := "ffm"

description := "Forest fire module"

organization := "cermb"

version := "1.0"

scalaVersion := "2.11.2"

// General dependencies
//
libraryDependencies ++= Seq(
  "com.vividsolutions" % "jts" % "1.13",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
  "org.scalautils" % "scalautils_2.11" % "2.1.5",
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
  "org.scalacheck" % "scalacheck_2.11" % "1.11.5" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test"
)

// Repositories
//
resolvers ++= Seq(
  "OSGeo Repository" at "http://download.osgeo.org/webdav/geotools/"
)


