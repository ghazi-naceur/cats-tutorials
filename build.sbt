ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(name := "cats-tutorials")

val catsVersion = "2.7.0"

libraryDependencies ++= Seq("org.typelevel" %% "cats-core" % catsVersion)
scalacOptions ++= Seq("-language:higherKinds")
