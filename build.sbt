name := "gpp"

version := "0.1"

organization := "edu.utexas"

scalaVersion := "2.10.1"

retrieveManaged := true

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "0.8.1",
  "org.scalanlp" % "chalk" % "1.1.3-SNAPSHOT",
  "org.scalanlp" % "nak" % "1.1.2"
)
