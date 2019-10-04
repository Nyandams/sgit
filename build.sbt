name := "sgit"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies += "com.github.scopt" % "scopt_2.12" % "4.0.0-RC2"

assemblyJarName in assembly := "sgit.jar"

test in assembly := {}

mainClass in assembly := Some("Application")