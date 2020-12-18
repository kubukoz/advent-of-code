val compilerPlugins = List(
  compilerPlugin("org.typelevel" % "kind-projector" % "0.11.1" cross CrossVersion.full),
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

val commonSettings = Seq(
  scalaVersion := "2.13.4",
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions += "-Ymacro-annotations",
  name := "scala",
  libraryDependencies ++= compilerPlugins
)

val cats = project
  .in(file("cats"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-io" % "3.0.0-M3",
      "org.typelevel" %% "cats-mtl" % "1.0.0",
      "org.typelevel" %% "cats-effect-std" % "3.0.0-M3",
      "org.typelevel" %% "cats-effect" % "3.0.0-M3",
      "org.typelevel" %% "cats-tagless-macros" % "0.12",
      "io.chrisdavenport" %% "semigroups" % "0.2.0",
      "io.chrisdavenport" %% "monoids" % "0.2.0",
      "io.estatico" %% "newtype" % "0.4.3",
      "com.github.julien-truffaut" %% "monocle-macro" % "2.0.0",
      "org.typelevel" %% "cats-parse" % "0.2.0",
      "org.tpolecat" %% "atto-core" % "0.7.0",
      "org.scalatest" %% "scalatest" % "3.2.3" % Test
    )
  )

val zio = project
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "1.0.3",
      "dev.zio" %% "zio-streams" % "1.0.3",
      "dev.zio" %% "zio-macros" % "1.0.3"
    )
  )

val root =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(skip in publish := true)
    .aggregate(cats, zio)
