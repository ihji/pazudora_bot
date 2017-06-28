name := "pazudora-bot"

scalaVersion := "2.11.11"

libraryDependencies += "com.github.pengrad" % "java-telegram-bot-api" % "1.3.2"

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "0.1.1"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.3"

libraryDependencies += "org.reactivemongo" %% "reactivemongo" % "0.12.4"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.19"

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

enablePlugins(GitVersioning)

git.useGitDescribe := true

enablePlugins(BuildInfoPlugin)

buildInfoOptions += BuildInfoOption.BuildTime

enablePlugins(JavaAppPackaging)
