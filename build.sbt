ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "DynamicProgramming",
    libraryDependencies += "log4j" % "log4j" % "1.2.14"
  )
