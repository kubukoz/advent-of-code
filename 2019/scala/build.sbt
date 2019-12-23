val compilerPlugins = List(
  compilerPlugin("org.typelevel" % "kind-projector"      % "0.11.0" cross CrossVersion.full),
  compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")
)

val commonSettings = Seq(
  scalaVersion := "2.13.1",
  scalacOptions ~= (_.filterNot(_ == "-Xfatal-warnings") ++ Seq("-Ymacro-annotations")),
  name := "scala",
  updateOptions := updateOptions.value.withGigahorse(false),
  libraryDependencies ++= Seq(
    "co.fs2"                     %% "fs2-io"           % "2.1.0",
    "dev.profunktor"             %% "console4cats"     % "0.8.0",
    "io.chrisdavenport"          %% "semigroups"       % "0.2.0",
    "io.chrisdavenport"          %% "monoids"          % "0.2.0",
    "com.olegpy"                 %% "meow-mtl-core"    % "0.4.0",
    "com.olegpy"                 %% "meow-mtl-effects" % "0.4.0",
    "io.estatico"                %% "newtype"          % "0.4.3",
    "com.github.julien-truffaut" %% "monocle-macro"    % "2.0.0",
    "org.tpolecat"               %% "atto-core"        % "0.7.2",
    "org.scalatest"              %% "scalatest"        % "3.1.0" % Test
  ) ++ compilerPlugins
)

val core = project.settings(commonSettings).settings(name += "-core")

val aoc =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(skip in publish := true)
    .dependsOn(core)
    .aggregate(core)
