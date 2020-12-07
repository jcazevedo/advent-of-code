ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "1.0"
ThisBuild / organization := "net.jcazevedo"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2020",
    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Xlint",
      "-Xfatal-warnings"
    ),
    scalafmtOnCompile := true
  )
