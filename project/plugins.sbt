addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.1")

resolvers ++= Seq(
  "Local Maven Repository" at s"${Path.userHome.asFile.toURI.toURL}/.m2/repository",
  "nexthink-releases" at "https://nexus.intra.nexthink.com/content/repositories/releases/",
  "nexthink-snapshots" at "https://nexus.intra.nexthink.com/content/repositories/snapshots/"
)

addSbtPlugin("com.nexthink" % "sbt-nxplugin" % "1.10.0")

val credFile = Path.userHome / ".sbt" / ".credentials"

val c = if (credFile.exists && credFile.isFile) List(Credentials(credFile)) else Nil

credentials ++= c
