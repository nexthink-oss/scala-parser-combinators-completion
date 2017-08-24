name := "scala-parser-combinators-completion"
organization := "com.nexthink"
licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
version := "1.0.0"
scalaVersion := "2.12.2"
bintrayRepository := "maven"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "com.novocode"           % "junit-interface"           % "0.11" % Test,
  "org.scalacheck"         %% "scalacheck"               % "1.13.4" % Test,
  "org.scalatest"          %% "scalatest"                % "3.0.1" % Test
)
