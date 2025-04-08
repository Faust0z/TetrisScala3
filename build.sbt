ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaTetris",
    version := "0.1",
    scalaVersion := "2.13.12",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      "org.scalatest" %% "scalatest" % "3.2.16" % Test,
      "com.typesafe.akka" %% "akka-actor" % "2.6.20"
    )
  )
