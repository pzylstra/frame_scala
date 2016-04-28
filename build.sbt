name := "ffm"

description := "Forest flammability model"

scalaVersion in ThisBuild := "2.11.8"

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
  aggregate(common, fire, forest, io, runner)

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
  settings(commonSettings: _*).
  settings(testSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "com.github.tototoshi" %% "scala-csv" % "1.3.1"
    )
  )

lazy val runner = (project in file("runner")).
  dependsOn(common, fire, forest, io).
  settings(commonSettings: _*).
  settings(testSettings: _*)


// For sbt-pack plugin
packAutoSettings

// Tell sbteclipse plugin to:
// - download source and create Eclipse attachments for dependencies
EclipseKeys.withSource := true
// - don't generate a project definition for the root project
EclipseKeys.skipParents in ThisBuild := true

