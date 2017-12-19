import scoverage.ScoverageKeys._

name := "scala-with-cats"
version := "1.0.0"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:higherKinds",
  "-Xlint",
  "-Xfatal-warnings",
  "-Ypartial-unification"
)

coverageMinimum       := 80
coverageFailOnMinimum := true

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0-MF",
  "org.scalatest" %% "scalatest" % "3.0.1"      % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
