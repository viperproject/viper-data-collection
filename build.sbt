import Dependencies._


lazy val carbon = project in file("carbon")
lazy val silicon = (project in file("silicon"))

lazy val root = (project in file("."))
  .dependsOn(carbon)
  .dependsOn(silicon)
  .settings(
    name := "viper-data-collection",
    organization := "viper",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.13.10",
    libraryDependencies += munit % Test,
    libraryDependencies += "com.lihaoyi" %% "upickle" % "1.3.0",
    libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.5",
    libraryDependencies ++= Seq(
      "com.typesafe.slick" %% "slick" % "3.3.3",
      "org.postgresql" % "postgresql" % "42.3.4",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.3.3",
      "com.github.tminglei" %% "slick-pg" % "0.20.3",
      "com.github.tminglei" %% "slick-pg_play-json" % "0.20.3"
    ),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test

  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
