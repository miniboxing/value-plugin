import sbt._
import Keys._
import Process._

object ValiumBuild extends Build {
  val defaults = Defaults.defaultSettings ++ Seq(

    scalaSource in Compile := baseDirectory.value / "src",
    javaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",
    javaSource in Test := baseDirectory.value / "test",
    resourceDirectory in Compile := baseDirectory.value / "resources",
    compileOrder := CompileOrder.JavaThenScala,

    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value),
    //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
    com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,

    resolvers in ThisBuild ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),

    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint"),

    publishArtifact in packageDoc := false,

    scalaHome := {
      val scalaHome = System.getProperty("valium.scala.home")
      if (scalaHome != null) {
        println(s"Going for custom scala home at $scalaHome")
        Some(file(scalaHome))
      } else None
    }
  )

  val pluginDeps = Seq(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )

  val junitDeps: Seq[Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      // "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
      "com.novocode" % "junit-interface" % "0.10-M2" % "test"
    ),
    parallelExecution in Test := false,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")
  )

  val testsDeps: Seq[Setting[_]] = junitDeps ++ Seq(
    fork in Test := true,
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      // Thanks Jason for this cool idea (taken from https://github.com/retronym/boxer)
      // add plugin timestamp to compiler options to trigger recompile of
      // main after editing the plugin. (Otherwise a 'clean' is needed.)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    },
    libraryDependencies ++= Seq(
      "com.googlecode.java-diff-utils" % "diffutils" % "1.2.1"
    )
  )

  lazy val valium      = Project(id = "valium",         base = file("."),                      settings = defaults) aggregate (runtime, plugin, tests)
  lazy val runtime     = Project(id = "valium-runtime", base = file("components/runtime"),     settings = defaults)
  lazy val plugin      = Project(id = "valium-plugin",  base = file("components/plugin"),      settings = defaults ++ pluginDeps) dependsOn(runtime)
  lazy val tests       = Project(id = "valium-tests",   base = file("tests/correctness"),      settings = defaults ++ pluginDeps ++ testsDeps) dependsOn(plugin, runtime)
}
