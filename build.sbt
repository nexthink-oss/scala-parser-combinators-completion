name := "scala-parser-combinators-completion"
organization := "com.nexthink"
licenses += ("BSD-3", url("https://opensource.org/licenses/bsd-3-clause"))
version := "1.0.9"
scalaVersion := "2.12.2"
homepage := Some(url("https://github.com/nexthink/scala-parser-combinators-completion"))
scmInfo := Some(ScmInfo(url(
  "https://github.com/nexthink/scala-parser-combinators-completion"),
  "git@github.com:nexthink/scala-parser-combinators-completions.git"))
developers := List(
  Developer(
    id="jchapuis",
    name="Jonas Chapuis",
    email="jonas.chapuis@nexthink.com",
    url=url("https://jonaschapuis.com")
  ))


// Add sonatype repository settings
isSnapshot := version.value endsWith "SNAPSHOT"
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

// Sonatype credentials
  credentials += Credentials(
    "Sonatype Nexus Repository Manager",
    "oss.sonatype.org",
    sys.env.getOrElse("SONATYPE_USER", ""),
    sys.env.getOrElse("SONATYPE_PASSWORD", ""))

// PGP signing
useGpg := false // built-in implementation
usePgpKeyHex("EDB397ECD91C486D")
pgpPublicRing := baseDirectory.value / "project" / ".gnupg" / "pubring.gpg"
pgpSecretRing := baseDirectory.value / "project" / ".gnupg" / "secring.gpg"
pgpPassphrase := sys.env.get("PGP_PASS").map(_.toArray)

addCommandAlias("ci-all",  ";+clean ;+compile ;+coverage ;+test ;+package ;+coverageReport ;+codacyCoverage")
addCommandAlias("release", ";+publishSigned ;sonatypeRelease")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.json4s"             %% "json4s-native"            % "3.5.3",
  "com.novocode"           % "junit-interface"           % "0.11" % Test,
  "org.scalacheck"         %% "scalacheck"               % "1.13.4" % Test,
  "org.scalatest"          %% "scalatest"                % "3.0.1" % Test
)