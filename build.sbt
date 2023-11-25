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
    libraryDependencies ++= Seq(
      //"org.slf4j" % "slf4j-nop" % "2.0.5",
      "com.lihaoyi" %% "cask" % "0.9.1",
      "com.lihaoyi" %% "upickle" % "3.0.0",
      "com.lihaoyi" %% "requests" % "0.8.0",
      "com.typesafe.slick" %% "slick" % "3.3.3",
      "org.postgresql" % "postgresql" % "42.3.4",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.3.3",
      "com.github.tminglei" %% "slick-pg" % "0.20.3",
      "com.github.tminglei" %% "slick-pg_play-json" % "0.20.3",
      "org.scalatest" %% "scalatest" % "3.2.11" % Test,
      "org.slf4j" % "slf4j-api" % "1.7.30", // Logging
      "ch.qos.logback" % "logback-classic" % "1.2.3"
    ),
    dependencyOverrides += "com.lihaoyi" %% "geny" % "1.0.0",
    dependencyOverrides += "org.slf4j" % "slf4j-nop" % "1.7.30",
    Test / fork := true

  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
