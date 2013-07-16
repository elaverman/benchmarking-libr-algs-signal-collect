import AssemblyKeys._
assemblySettings

/** Project **/
name := "signal-collect-dcops"

version := "2.0.0-SNAPSHOT"

fork in run := true

organization := "com.signalcollect"

scalaVersion := "2.10.0-RC5"

resolvers += "Typesafe Snapshot Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

/** Dependencies */
libraryDependencies ++= Seq(
 "org.scala-lang" % "scala-library" % "2.10.0-RC5"  % "compile",
 "junit" % "junit" % "4.8.2"  % "test",
 "org.specs2" % "specs2_2.10.0-RC5" % "1.12.3"  % "test",
 "net.sf.opencsv" % "opencsv" % "2.3",
 "org.scalatest" % "scalatest_2.10" % "1.9.1"
)
