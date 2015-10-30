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
    version := "0.2-SNAPSHOT",
    description := "A simple vector maths library for graphics programming.",
    homepage := Some(url("https://github.org/mseddon/vecmath")),
    scmInfo := Some(ScmInfo(
      url("https://github.com/mseddon/vecmath"),
          "scm:git:git@github.com:mseddon/vecmath.git",
      Some("scm:git:git@github.com:mseddon/vecmath.git"))),
    publishMavenStyle := true,
    licenses := Seq(("BSD-3-Clause", url("http://opensource.org/licenses/BSD-3-Clause"))),
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { _ => false },
    pomExtra := (
      <developers>
        <developer>
          <id>mseddon</id>
          <name>Matt Seddon</name>
          <url>https://github.com/mseddon/</url>
        </developer>
      </developers>
      )
  )

lazy val vecMathJVM = vecMath.jvm
lazy val vecMathJS = vecMath.js