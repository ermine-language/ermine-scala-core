name := "ermine"

resolvers += "Typesafe Sonatype Snapshots" at "http://repo.typesafe.com/typesafe/sonatype-snapshots/"

scalaVersion := "2.10.1"

libraryDependencies += "bound" %% "bound" % "0.1-SNAPSHOT"

crossScalaVersions := Seq("2.9.2", "2.9.3", "2.10.0", "2.10.1")

scalacOptions <++= (scalaVersion) map { sv =>
  val versionDepOpts =
    if (sv startsWith "2.9") Seq()
    else Seq("-feature", "-language:higherKinds", "-language:implicitConversions")
  Seq("-deprecation", "-unchecked") ++ versionDepOpts
}
