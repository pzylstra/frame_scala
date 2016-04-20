// To generate an Eclipse project
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "4.0.0")

// To generate a fat jar containing all dependencies
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.1")

// To collect all required jars (alternative to a single fat jar)
addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.7.9")
 
