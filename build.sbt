name := "advent-of-code-2018"

version := "1.0"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "io.chrisdavenport" %% "cats-par" % "0.2.0",
  "org.tpolecat" %% "atto-core" % "0.6.4",
  "org.typelevel" %% "cats-core" % "1.5.0",
  "org.typelevel" %% "mouse" % "0.19",
  "com.github.gvolpe" %% "console4cats" % "0.5",
  "org.typelevel" %% "cats-effect" % "1.1.0",
  "co.fs2" %% "fs2-io" % "1.0.0",
  "com.beachape" %% "enumeratum" % "1.5.12",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.softwaremill.quicklens" %% "quicklens" % "1.4.11",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.olegpy" %% "meow-mtl" % "0.2.0",
  "io.chrisdavenport" %% "monoids" % "0.0.2",
  "com.github.julien-truffaut" %% "monocle-macro" % "1.5.0-cats",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8"),
  compilerPlugin(
    "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
  ),
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")
)

scalacOptions ++= Seq("-language:higherKinds", "-Ypartial-unification")
