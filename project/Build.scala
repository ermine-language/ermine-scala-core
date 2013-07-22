import sbt._
import Keys._

object ErmineScalaCoreBuild extends Build {

  lazy val ermine = Project( id = "ermine-scala-core" , base = file(".")).dependsOn(machines, f0, boundCore, boundF0, boundSCB)

  lazy val machines  = fromGithub("runarorama", "scala-machines")
  lazy val boundCore = fromGithub("runarorama", "scala-bound", subProject = Some("core"))
  lazy val boundF0   = fromGithub("runarorama", "scala-bound", subProject = Some("bound-f0-binding"))
  lazy val boundSCB  = fromGithub("runarorama", "scala-bound", subProject = Some("bound-scalacheck-binding"))
  lazy val f0        = fromGithub("joshcough" , "f0")

  // force sbt to get the latest version of a project
  // 'sbt update' doesn't seem to get the latest even though this says that it should
  // http://stackoverflow.com/questions/8864317/how-do-i-refresh-updated-git-dependency-artifacts-in-sbt
  // so instead we have to go to github and get the latest version.
  def fromGithub(githubUser: String, project: String, subProject: Option[String] = None) = {
    val commitsUrl = "https://api.github.com/repos/"+githubUser+"/"+project+"/commits?sha=master"
    val sha = scala.io.Source.fromURL(commitsUrl).takeWhile(_ != ',').mkString.dropWhile(_!=':').drop(2).dropRight(1)
    val projectUri = uri("https://github.com/"+githubUser+"/"+project+".git#" + sha)
    subProject match {
      case None => RootProject(projectUri)
      case Some(sub) => ProjectRef(projectUri, sub)
    }
  }
}