name := "wichcraft"

version := "0.1"

scalaVersion := "2.10.0"

organization := "com.botequim"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"/*, "-Xprint:typer"*/)

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-swing" % "2.10.0",
    "com.miglayout" % "miglayout-swing" % "4.2",
    "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)
