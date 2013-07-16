import sbt._
import Keys._

object DcopsBuild extends Build {
   //lazy val scCore = ProjectRef(file("../signal-collect"), id = "signal-collect")
   val scDcops = Project(id = "signal-collect-dcops", base = file("."))// dependsOn(scCore)
}
