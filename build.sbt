lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.example",
      scalaVersion := "2.12.4",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "PTS",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test
  )
