
val scalaVer = "2.12.7"
val silhouetteVersion = "5.0.6"

/**
  * Macros project
  */
lazy val macros = (project in file("macros")).settings(
  name := "macros",
  version := "0.0.1",
  scalaVersion := scalaVer,
  resolvers ++= Seq (
    Resolver.jcenterRepo,
    "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  ),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVer,
    "org.scala-lang" % "scala-compiler" % scalaVer,
    // Cache
    "com.aerospike" % "aerospike-client" % "4.2.2",
    // ArangoDB
    "com.arangodb" % "arangodb-java-driver-async" % "5.0.3"
  )
)

/**
  * Database drivers (ArangoDB and Aerospike)
  */
lazy val drivers = (project in file("drivers")).settings(
  name := "drivers",
  version := "0.0.1",
  scalaVersion := scalaVer,
  resolvers ++= Seq (
    Resolver.jcenterRepo,
    "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  ),
  libraryDependencies ++= Seq(
    guice,
    // Convert functions between java8 classes and scala
    "org.scala-lang.modules" %% "scala-java8-compat" % "0.+"
  )
).dependsOn(macros)

/**
  * Main project
  */
lazy val root = (project in file(".")).enablePlugins(PlayScala).settings(
  scalaVersion := scalaVer,
  name := """orgma""",
  organization := "org.sprig",
  version := "1.0-SNAPSHOT",
  
  resolvers ++= Seq (
    Resolver.jcenterRepo,
    "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  ),
  
  libraryDependencies ++= Seq(
    guice,
  
    // For guice DI, use it to avoid java type erasure within guice
    "net.codingwell" %% "scala-guice" % "4.2.1",
    
    // Authentication library for play framework
    "com.mohiva" %% "play-silhouette" % silhouetteVersion,
  
    "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
    "com.mohiva" %% "play-silhouette-testkit" %  silhouetteVersion %  Test
//    "com.playtika.testcontainers" % "embedded-aerospike" % "1.+" % Test
  )
).dependsOn(drivers)

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "org.sprig.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "org.sprig.binders._"
