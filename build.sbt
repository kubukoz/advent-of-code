name := "advent-of-code-2018"

version := "1.0"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.5.0",
  "org.typelevel" %% "mouse" % "0.19",
  "com.github.gvolpe" %% "console4cats" % "0.5",
  "org.typelevel" %% "cats-effect" % "1.0.0",
  "co.fs2" %% "fs2-io" % "1.0.0",
  "com.beachape" %% "enumeratum" % "1.5.12",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.softwaremill.quicklens" %% "quicklens" % "1.4.11",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

scalacOptions ++= Seq("-language:higherKinds", "-Ypartial-unification")
