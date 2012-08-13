name := "wichcraft"

version := "0.1"

scalaVersion := "2.9.2"

organization := "com.botequim"

scalacOptions ++= Seq("-unchecked", "-deprecation"/*, "-Xprint:typer"*/)

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-swing" % "2.9.2",
    "com.miglayout" % "miglayout-swing" % "4.2",
    "ch.qos.logback" % "logback-classic" % "1.0.6" % "runtime",
    "com.weiglewilczek.slf4s" % "slf4s_2.9.1" % "1.0.7",
    "org.scalatest" %% "scalatest" % "1.8" % "test"
)
