name := "PadletCat"

version := "0.1"

scalaVersion := "2.11.7"

// https://mvnrepository.com/artifact/com.typesafe.akka/akka-http
val akkaHttpVersion = "1.0-RC4"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http-core-experimental" % akkaHttpVersion,
  "org.scalafx" %% "scalafx" % "8.0.144-R12",
  "org.java-websocket" % "Java-WebSocket" % "1.3.0",
  "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "mysql" % "mysql-connector-java" % "5.1.24",
  "com.typesafe.akka" %% "akka-actor" % "2.4.0",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.10.1"
)
