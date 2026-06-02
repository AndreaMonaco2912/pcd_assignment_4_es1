val scala3Version = "3.8.3"
val PekkoVersion = "1.6.0"

ThisBuild / scalaVersion := scala3Version

ThisBuild / libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.20" % Test,
  "org.apache.pekko" %% "pekko-actor-typed" % PekkoVersion,
  "org.apache.pekko" %% "pekko-actor-testkit-typed" % PekkoVersion % Test,
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
  "ch.qos.logback" % "logback-classic" % "1.5.34"
)

lazy val root = (project in file("."))
  .settings(
    name := "SmartHomeAlarm",
    idePackagePrefix := Some("assignment4")
  )