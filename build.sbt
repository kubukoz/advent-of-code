name := "advent-of-code"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

val akkaV = "2.4.1"
val akkaStreamV = "2.0-M2"
val scalaTestV = "2.2.5"

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor_2.11" % "2.4.1",
  "com.typesafe.akka" %% "akka-actor" % akkaV,
  "com.typesafe.akka" %% "akka-remote" % akkaV,
  "org.scalatest" %% "scalatest" % scalaTestV % "test"
)