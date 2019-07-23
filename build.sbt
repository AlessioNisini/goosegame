name := "goosegame"

version := "0.1"

scalaVersion := "2.12.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies += "org.mockito" % "mockito-core" % "3.0.0" % Test

parallelExecution in ThisBuild := false