
name := "gitter-getter"

scalaVersion := "2.12.4"

organization := "org.adamchandra"

val circeJsonVersion    = "0.9.1"
val scalazVersion       = "7.2.20"
val ammoniteVersion     = "1.0.3"

libraryDependencies ++= Seq(
  "org.scalaj"  %% "scalaj-http"    % "2.3.0",
  "io.circe"    %% "circe-generic"  % circeJsonVersion,
  "io.circe"    %% "circe-parser"   % circeJsonVersion,
  "io.circe"    %% "circe-literal"  % circeJsonVersion,
  "org.scalaz"  %% "scalaz-core"    % scalazVersion,
  "com.lihaoyi" %% "ammonite-ops"    % ammoniteVersion
)

