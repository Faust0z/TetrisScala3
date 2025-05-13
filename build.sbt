ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaTetris",
    version := "0.1",
    scalaVersion := "3.3.1",
    Compile / mainClass := Some("scalatetris.Main"),
    // Configuración de Scaladoc
    Compile / doc / scalacOptions ++= Seq(
      "-doc-title", "ScalaTetris Documentation",
      "-doc-version", "0.1",
      "-doc-footer", "Tetris implementation in Scala",
      "-groups",
      "-doc-root-content", baseDirectory.value + "/src/main/scala/root-doc.txt"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "com.typesafe.akka" %% "akka-actor-typed" % "2.8.8",
      "org.slf4j" % "slf4j-simple" % "2.0.17"
    ),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", "versions", _@_*) => MergeStrategy.discard
      case PathList("reference.conf") => MergeStrategy.concat
      case PathList("META-INF", name@_*) =>
        name.map(_.toLowerCase) match {
          case Seq("manifest.mf") | Seq("index.list") => MergeStrategy.discard
          case _ => MergeStrategy.first
        }
      case _ => MergeStrategy.first
    }
  )