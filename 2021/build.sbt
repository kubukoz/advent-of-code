val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-macro" % "3.1.0",
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-mtl" % "1.2.1",
      "co.fs2" %% "fs2-core" % "3.2.4",
      "org.typelevel" %% "cats-testkit-scalatest" % "2.1.5" % Test,
      compilerPlugin(("org.polyvariant" % "better-tostring" % "0.3.11").cross(CrossVersion.full)),
      compilerPlugin(("org.typelevel" % "kind-projector" % "0.13.2").cross(CrossVersion.full)),
    ),
    scalaVersion := "2.13.7",
    scalacOptions ++= Seq(
      "-Ywarn-unused:imports",
      "-Xsource:3.0",
    ),
  )
