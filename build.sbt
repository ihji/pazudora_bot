name := "pazudora-bot"

scalaVersion := "2.12.8"

libraryDependencies += "com.github.pengrad" % "java-telegram-bot-api" % "1.3.2"

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.1.0"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.3"

libraryDependencies += "org.reactivemongo" %% "reactivemongo" % "0.17.1"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.19"

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

enablePlugins(GitVersioning)

git.useGitDescribe := true

enablePlugins(BuildInfoPlugin)

buildInfoOptions += BuildInfoOption.BuildTime

enablePlugins(JavaAppPackaging)
