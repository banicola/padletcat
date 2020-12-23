import Dependencies._

ThisBuild / scalaVersion := "2.11.7"
ThisBuild / version := "0.1"
ThisBuild / organization := "be.master2"
ThisBuild / organizationName := "groupe4"

scalacOptions := Seq("-unchecked", "-deprecation")

lazy val root = (project in file("."))
  .settings(
    name := "bachT",
    libraryDependencies += scalaTest                      % Test,
    libraryDependencies += "org.scala-lang.modules"       %% "scala-parser-combinators" % "1.0.2",
    libraryDependencies += "org.scala-lang.modules"       % "scala-swing_2.11" % "1.0.1",
    libraryDependencies += "org.scalafx"                  %% "scalafx" % "8.0.144-R12",
    libraryDependencies += "com.typesafe.akka"            %% "akka-actor" % "2.4.0",
    libraryDependencies += "com.typesafe.akka"            %% "akka-http-core-experimental" % "2.0.2",
    libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.10.1",
    libraryDependencies += "mysql"                        % "mysql-connector-java" % "8.0.13"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
