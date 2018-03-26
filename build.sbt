import org.ensime.EnsimeCoursierKeys._
import org.ensime.EnsimeKeys._

name := "figaro-examples"

version := "0.1"

scalaVersion in ThisBuild := "2.12.5"

lazy val root = (project in file(".")).settings(
  libraryDependencies ++= Seq(
    "com.cra.figaro" %% "figaro" % "5.0.0.0"))
