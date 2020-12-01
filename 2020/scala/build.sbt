val commonSettings = Seq(
  scalacOptions -= "-Xfatal-warnings",
  name := "scala"
)

val cats = project
  .in(file("cats"))
  .settings(commonSettings)
  .settings(
    scalaVersion := "3.0.0-M2",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-io" % "3.0.0-M4",
      // ("org.typelevel" %% "cats-mtl" % "1.0.0").withDottyCompat(scalaVersion.value),
      "org.typelevel" %% "cats-effect-std" % "3.0.0-M4",
      "org.typelevel" %% "cats-effect" % "3.0.0-M4",
      // "io.chrisdavenport" %% "semigroups" % "0.2.0",
      // "io.chrisdavenport" %% "monoids" % "0.2.0",
      "org.scalatest" %% "scalatest" % "3.2.3" % Test
    )
  )

val zio = project
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.13.4",
    scalacOptions += "-Ymacro-annotations",
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
