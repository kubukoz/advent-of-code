val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0",
      compilerPlugin(("org.polyvariant" % "better-tostring" % "0.3.11").cross(CrossVersion.full)),
      compilerPlugin(("org.typelevel" % "kind-projector" % "0.13.2").cross(CrossVersion.full)),
    ),
    scalaVersion := "2.13.7",
    scalacOptions ++= Seq(
      "-Ywarn-unused:imports"
    ),
  )
