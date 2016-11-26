name := "bittorrent"

version := "1.0"

scalaVersion := "2.11.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4" withSources()
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.14" withSources()
libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.0.0" withSources()
