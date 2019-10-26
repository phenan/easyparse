name := "easyparse"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.13.1",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

scalacOptions ++= Seq("-deprecation", "-feature")
