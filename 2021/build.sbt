val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-mtl" % "1.2.1",
      compilerPlugin(("org.polyvariant" % "better-tostring" % "0.3.11").cross(CrossVersion.full)),
    ),
    scalacOptions += "-Ykind-projector",
    scalaVersion := "3.1.0",
  )
