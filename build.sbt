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
    libraryDependencies += "com.typesafe.slick" %% "slick" % "3.5.0-M4",
    libraryDependencies += "mysql" % "mysql-connector-java" % "8.0.33",
    libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.5",
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
