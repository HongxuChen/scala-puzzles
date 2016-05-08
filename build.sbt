name := "puzzles"

version := "1.0"

scalaVersion := "2.11.8"

val scalazVersion = "7.2.2"

scalacOptions += "-feature"


libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
  "com.github.wheaties" % "autolift-scalaz_2.11" % "0.5"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0-M16-SNAP3" % "test"
)

initialCommands in console := "import scalaz._, Scalaz._"