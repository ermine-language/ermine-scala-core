import sbt._

object ScalaErmine extends Build {
  lazy val root = Project("root", file(".")) dependsOn(f0)
  lazy val f0 = RootProject(uri("https://github.com/joshcough/f0.git"))
}