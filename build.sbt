name := "scala-parser-combinators-completion"
organization := "com.nexthink"
licenses += ("BSD-3", url("https://opensource.org/licenses/bsd-3-clause"))
version := "1.0.8"
scalaVersion := "2.12.2"

// Add sonatype repository settings
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.json4s"             %% "json4s-native"            % "3.5.3",
  "com.novocode"           % "junit-interface"           % "0.11" % Test,
  "org.scalacheck"         %% "scalacheck"               % "1.13.4" % Test,
  "org.scalatest"          %% "scalatest"                % "3.0.1" % Test
)