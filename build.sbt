
name := "profanity-filter"

version := "1.0"

scalaVersion := "2.12.1"

retrieveManaged := true

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
  "-Ywarn-unused"
)


libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.10",
  "com.typesafe.play" %% "play-json" % "2.6.3",
  "com.amazonaws" % "aws-lambda-java-core" % "1.1.0",
  "com.amazonaws" % "aws-lambda-java-events" % "1.3.0",
  "com.amazonaws" % "aws-java-sdk-s3" % "1.11.189",
  "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.195"
)


javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")


assemblyMergeStrategy in assembly := {
  {
    case PathList("META-INF", xs@_*) => MergeStrategy.discard
    case x => MergeStrategy.first
  }
}

