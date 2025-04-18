ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaTetris",
    version := "0.1",
    scalaVersion := "3.3.1",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "com.typesafe.akka" %% "akka-actor-typed" % "2.8.8",
      "org.slf4j" % "slf4j-simple" % "2.0.17"
    )
  )
