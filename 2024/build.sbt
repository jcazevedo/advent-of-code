ThisBuild / scalaVersion := "3.5.2"

lazy val root = (project in file("."))
  .settings(
    name              := "advent-of-code-2024",
    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-Xfatal-warnings"
    ),
    scalafmtOnCompile := true
  )
