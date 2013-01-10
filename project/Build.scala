import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "1.0.0",
    scalaVersion := "2.10.0",
    scalacOptions ++= Seq(),
	libraryDependencies += "org.specs2" %% "specs2" % "1.13" % "test",

    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
	
	scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", 
	//"-language:implicitConversions","-Ymacro-debug-lite")
	"-language:implicitConversions")
	
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("core"),
    settings = buildSettings
  ) aggregate(macros, core)

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _))
    
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings
  ) dependsOn(macros)
}