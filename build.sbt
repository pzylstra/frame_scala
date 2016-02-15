name := "ffm"

description := "Forest flammability model"

scalaVersion in ThisBuild := "2.11.7"

lazy val printClasspath = taskKey[Unit]("Print classpath")

lazy val commonSettings = Seq(
  version := "0.1",
  organization := "cermb",
  printClasspath := {
    val els = (fullClasspath in Runtime).value.files map(_.getPath)
    print(els.mkString(java.io.File.pathSeparator))
  }
)

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
    "org.mockito" % "mockito-core" % "1.9.5" % "test"
  )
)

lazy val root = (project in file(".")).
  aggregate(common, fire, forest, io, spike)

lazy val common = (project in file("common")).
  settings(commonSettings: _*).
  settings(testSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.vividsolutions" % "jts" % "1.13",
      "org.apache.commons" % "commons-math3" % "3.3"
    )
  )

lazy val forest = (project in file("forest")).
  dependsOn(common).
  settings(commonSettings: _*).
  settings(testSettings: _*)

lazy val fire = (project in file("fire")).
  dependsOn(common, forest).
  settings(commonSettings: _*).
  settings(testSettings: _*)

lazy val io = (project in file("io")).
  dependsOn(common, forest).
  settings(testSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
    )
  )

lazy val spike = (project in file("spike")).
  dependsOn(common, fire, forest, io).
  settings(
    libraryDependencies ++= Seq(
      "com.typesafe.slick" %% "slick" % "3.1.1",
      "org.xerial" % "sqlite-jdbc" % "3.7.2",         // SQLite JDBC driver
      "org.slf4j" % "slf4j-nop" % "1.6.4" 
    )
  )

// Tell sbteclipse plugin to:
// - download source and create Eclipse attachments for dependencies
EclipseKeys.withSource := true
// - don't generate a project definition for the root project
EclipseKeys.skipParents in ThisBuild := true

