import sbt.Keys._

lazy val root = project.in(file(".")).
  aggregate(vecMathJS, vecMathJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val vecMath = crossProject.in(file(".")).
  settings(
    scalaVersion := "2.11.6",
    organization := "com.scryetek",
    name := "vecmath",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.6"
  )

lazy val vecMathJVM = vecMath.jvm
lazy val vecMathJS = vecMath.js