name := "profanity-filter"

version := "1.0"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  "-feature",
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused"
)


libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.10",
  "com.typesafe.akka" %% "akka-http-core" % "10.0.9",
  "com.typesafe.play" %% "play-ws" % "2.6.3"
)

//resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

