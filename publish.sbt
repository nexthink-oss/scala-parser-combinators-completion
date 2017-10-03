resolvers ++= Seq(
  "Local Maven Repository" at s"${Path.userHome.asFile.toURI.toURL}/.m2/repository",
  "nexthink-releases" at "https://nexus.intra.nexthink.com/content/repositories/releases/",
  "nexthink-snapshots" at "https://nexus.intra.nexthink.com/content/repositories/snapshots/"
)

publishMavenStyle := true

pomIncludeRepository := { _ => false }

def publishTarget(isSnapshot: Boolean): Some[sbt.MavenRepository] = {
  val nexus = "https://nexus.intra.nexthink.com/"
  if (isSnapshot)
    Some("nexus-snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("nexus-releases" at nexus + "content/repositories/releases")
}

publishTo := publishTarget(isSnapshot.value)

val credFile = Path.userHome / ".sbt" / ".credentials"

val c = if (credFile.exists && credFile.isFile) List(Credentials(credFile)) else Nil

credentials ++= c

pomExtra := (
  <developers>
    <developer>
      <name>Nexthink SA</name>
      <url>http://nexthink.com</url>
    </developer>
  </developers>)

// use maven/aether for publishing
overridePublishBothSettings

