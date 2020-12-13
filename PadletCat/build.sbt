name := "PadletCat"

version := "0.1"

scalaVersion := "2.11.7"

// https://mvnrepository.com/artifact/com.typesafe.akka/akka-http
val akkaHttpVersion = "1.0-RC4"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http-experimental" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream-experimental" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-core-experimental" % akkaHttpVersion,
  "org.java-websocket" % "Java-WebSocket" % "1.3.0",
  "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

)
