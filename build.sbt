name := "pazudora-bot"

version := "0.2"

scalaVersion := "2.11.7"

libraryDependencies += "com.github.pengrad" % "java-telegram-bot-api" % "1.3.2"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.3.0"

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "0.1.1"

libraryDependencies += "com.twitter" %% "finagle-http" % "6.33.0"

enablePlugins(JavaAppPackaging)
