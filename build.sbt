name := "wichcraft"

version := "0.1"

scalaVersion := "2.9.1"

organization := "com.botequim"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-swing" % "2.9.1",
    "ch.qos.logback" % "logback-classic" % "0.9.25" % "runtime",
    "com.weiglewilczek.slf4s" %% "slf4s" % "1.0.7",
    "org.scalatest" %% "scalatest" % "1.7.1" % "test"
)
