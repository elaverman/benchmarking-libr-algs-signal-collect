import AssemblyKeys._
assemblySettings

/** Project **/
name := "libr-algs"

version := "2.1.0-SNAPSHOT"

fork in run := true

organization := "com.signalcollect"

scalaVersion := "2.10.3"

resolvers += "Typesafe Snapshot Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

resolvers += "Scala-Tools Repository" at "https://oss.sonatype.org/content/groups/scala-tools/"

resolvers += "Sonatype Snapshots Repository" at "https://oss.sonatype.org/content/repositories/snapshots/"

/** Dependencies */
libraryDependencies ++= Seq(
 "org.scala-lang" % "scala-library" % "2.10.3"  % "compile",
 "junit" % "junit" % "4.8.2"  % "test",
 "org.specs2" %% "specs2" % "2.0-RC2" % "test",
 "net.sf.opencsv" % "opencsv" % "2.3",
 "org.scalatest" %% "scalatest" % "2.0.M7" % "test"
)
