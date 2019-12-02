val compilerPlugins = List(
  compilerPlugin("org.scalamacros" % "paradise"            % "2.1.1" cross CrossVersion.full),
  compilerPlugin("org.typelevel"   % "kind-projector"      % "0.11.0" cross CrossVersion.full),
  compilerPlugin("com.olegpy"      %% "better-monadic-for" % "0.3.1")
)

val commonSettings = Seq(
  scalaVersion := "2.12.10",
  scalacOptions ~= (_.filterNot(_ == "-Xfatal-warnings")),
  fork in Test := true,
  name := "scala",
  updateOptions := updateOptions.value.withGigahorse(false),
  libraryDependencies ++= Seq(
    "co.fs2"                     %% "fs2-io"        % "2.1.0",
    "dev.profunktor"             %% "console4cats"  % "0.8.0",
    "io.estatico"                %% "newtype"       % "0.4.3",
    "com.github.julien-truffaut" %% "monocle-macro" % "2.0.0",
    "org.scalatest"              %% "scalatest"     % "3.0.8" % Test
  ) ++ compilerPlugins
)

val core = project.settings(commonSettings).settings(name += "-core")

val scala =
  project.in(file(".")).settings(commonSettings).settings(skip in publish := true).dependsOn(core).aggregate(core)
