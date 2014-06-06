scalaVersion := "2.11.0"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "2.3.12" % "test",
                "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
                "com.netflix.rxjava" % "rxjava-scala" % "0.18.3",
                "io.spray" %%  "spray-json" % "1.2.6")

resolvers += "releases"  at "http://oss.sonatype.org/content/repositories/releases"

resolvers += "spray" at "http://repo.spray.io/"

scalacOptions ++= Seq("-deprecation", "-feature")

