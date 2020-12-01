ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "1.0"
ThisBuild / organization     := "net.jcazevedo"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2020"
  )
