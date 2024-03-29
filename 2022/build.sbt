ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name              := "advent-of-code-2022",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
    ),
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
