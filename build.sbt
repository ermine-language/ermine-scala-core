name := "ermine"

resolvers += "Typesafe Sonatype Snapshots" at "http://repo.typesafe.com/typesafe/sonatype-snapshots/"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT",
  "clarifi" %% "f0" % "0.2-SNAPSHOT",
  "bound" %% "bound-core" % "0.2-SNAPSHOT",
  "bound" %% "bound-scalacheck-binding" % "0.2-SNAPSHOT",
  "bound" %% "bound-f0-binding" % "0.2-SNAPSHOT",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
)

crossScalaVersions := Seq("2.9.2", "2.9.3", "2.10.0", "2.10.1")

scalacOptions <++= (scalaVersion) map { sv =>
  val versionDepOpts =
    if (sv startsWith "2.9") Seq()
    else Seq("-feature", "-language:higherKinds", "-language:implicitConversions")
  Seq("-deprecation", "-unchecked") ++ versionDepOpts
}

initialCommands in console := "import scalaz._, Scalaz._"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-s", "1000")