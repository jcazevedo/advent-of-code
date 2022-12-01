ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name              := "advent-of-code-2022",
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
