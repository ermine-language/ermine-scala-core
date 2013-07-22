name := "scala-ermine-core"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.2",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
)

scalacOptions ++=
  Seq("-deprecation", "-unchecked", "-feature", "-language:higherKinds", "-language:implicitConversions")

initialCommands in console := "import scalaz._, Scalaz._"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-s", "1000")
