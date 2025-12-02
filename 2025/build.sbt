ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name              := "advent-of-code-2025",
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
