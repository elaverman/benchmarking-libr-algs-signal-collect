import sbt._
import Keys._

object DcopsBuild extends Build {
   //lazy val scCore = ProjectRef(file("../signal-collect"), id = "signal-collect")
   val scDcops = Project(id = "libr-algs", base = file("."))// dependsOn(scCore)
}
