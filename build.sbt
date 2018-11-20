import scalariform.formatter.preferences._

name := "adventofcode-2015"

organization := "net.jcazevedo"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "io.spray" %%  "spray-json" % "1.3.2")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:implicitConversions")

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignParameters, true)
  .setPreference(DoubleIndentClassDeclaration, true)
