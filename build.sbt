name := "gpp"

version := "0.1"

organization := "edu.utexas"

scalaVersion := "2.10.1"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.rogach" %% "scallop" % "0.8.1"

libraryDependencies += "org.scalanlp" % "chalk" % "1.1.3-SNAPSHOT"

libraryDependencies += "org.scalanlp" % "nak" % "1.1.2-SNAPSHOT"
