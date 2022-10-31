ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.9"

lazy val root = (project in file("."))
  .settings(
    name := "raytracer"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"
