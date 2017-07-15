name := "free-fun"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.2"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats"      % "0.9.0",
  "org.typelevel" %% "cats-free" % "0.9.0",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test"
)


