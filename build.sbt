name := "Boolean-Riddles"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions := Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.12" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+",
)
