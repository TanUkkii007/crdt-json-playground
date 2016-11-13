name := "crdt-json-playground"

scalaVersion := "2.11.8"

libraryDependencies := Seq(
  "eu.timepit" %% "crjdt-core"  % "0.0.5",
  "io.circe" %% "circe-core" % "0.6.0",
  "io.circe" %% "circe-generic" % "0.6.0",
  "io.circe" %% "circe-parser" % "0.6.0",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)
