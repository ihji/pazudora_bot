name := "pazudora-bot"

version := "0.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "info.mukel" %% "telegrambot4s" % "1.0.3-SNAPSHOT"

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "0.1.1"

libraryDependencies += "com.twitter" %% "finagle-http" % "6.33.0"

enablePlugins(JavaAppPackaging)
