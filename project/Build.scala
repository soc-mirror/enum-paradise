import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.github.aloiscochard.enum-paradise",
    version := "0.3-SNAPSHOT",
    scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint"),
    scalaVersion := "2.11.0-SNAPSHOT",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise_2.11.0-SNAPSHOT" % "2.0.0-SNAPSHOT")
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in core
    )
  ) aggregate(macros, core)

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _))
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings
  ) dependsOn(macros)
}
