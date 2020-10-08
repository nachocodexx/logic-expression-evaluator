name := "logic-expression-evaluator"
organization := "io.codex"

version := "0.1"

scalaVersion := "2.13.3"

val http4sVersion = "0.21.6"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "2.2.0",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.scalatest" %% "scalatest" % "3.2.0" % Test,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  // Optional for auto-derivation of JSON codecs
  "io.circe" %% "circe-generic" % "0.13.0",
  // Optional for string interpolation to JSON model
  "io.circe" %% "circe-literal" % "0.13.0",
  "org.slf4j" % "slf4j-simple" % "1.6.4"
)
enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)
dockerExposedPorts ++= Seq(8080,8080)
