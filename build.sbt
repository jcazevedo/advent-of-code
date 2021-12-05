import scalariform.formatter.preferences._

name := "adventofcode-2017"

organization := "net.jcazevedo"

version := "1.0"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:implicitConversions")

scalariformPreferences := scalariformPreferences.value
  .setPreference(AlignParameters, true)
  .setPreference(DoubleIndentConstructorArguments, true)
