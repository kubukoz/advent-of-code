name := "advent-of-code"

version := "1.0"

scalaVersion := "2.12.0"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % "2.4.14",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
