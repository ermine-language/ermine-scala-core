name := "scala-ermine-core"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"               % "7.0.3",
  "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.0.3" % "test",
  "org.scalacheck" %% "scalacheck"                % "1.10.1" % "test",
  "machines"       %% "machines"                  % "1.0",
  "com.clarifi"    %% "f0"                        % "1.1",
  "bound"          %% "bound-core"                % "1.2",
  "bound"          %% "bound-f0-binding"          % "1.2",
  "bound"          %% "bound-scalacheck-binding"  % "1.2" % "test"
)

scalacOptions ++=
  Seq("-deprecation", "-unchecked", "-feature", "-language:higherKinds", "-language:implicitConversions")

initialCommands in console := "import scalaz._, Scalaz._"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-s", "1000")

traceLevel := 10

seq(bintraySettings:_*)

bintray.Keys.bintrayOrganization in bintray.Keys.bintray := Some("ermine")
