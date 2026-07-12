// Frozen strict scalac set carried over verbatim from the previous build tool.
val commonScalacOptions = Seq(
  "-Wnonunit-statement",
  "-Wunused:explicits",
  "-Wunused:implicits",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:params",
  "-Wunused:privates",
  "-Wvalue-discard",
  "-Xfatal-warnings",
  "-Ykind-projector",
  "-deprecation",
  "-encoding",
  "utf8",
  "-feature",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked"
)

lazy val client = (project in file("client"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "client",
    scalaVersion := "3.3.8",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    Compile / scalaSource := baseDirectory.value / "src",
    Compile / resourceDirectory := baseDirectory.value / "src" / "resources",
    Compile / mainClass := Some("chat.ChatClient"),
    Universal / packageName := "client",
    Universal / maintainer := "mugge",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Version.catsCoreVersion,
      "org.typelevel" %% "cats-effect" % Version.catsEffectVersion,
      "org.typelevel" %% "cats-mtl" % Version.catsMtlVersion,
      "co.fs2" %% "fs2-core" % Version.fs2Version,
      "co.fs2" %% "fs2-io" % Version.fs2Version,
      "com.comcast" %% "ip4s-core" % Version.ip4sVersion,
      "com.softwaremill.sttp.client3" %% "core" % Version.sttpVersion,
      "com.softwaremill.sttp.client3" %% "cats" % Version.sttpVersion,
      "org.typelevel" %% "log4cats-slf4j" % Version.log4catsVersion,
      "ch.qos.logback" % "logback-classic" % Version.logbackVersion
    ),
    scalacOptions ++= commonScalacOptions
  )
