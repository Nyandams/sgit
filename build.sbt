import sbtassembly.AssemblyPlugin.defaultUniversalScript

name := "sgit"

version := "0.1"

scalaVersion := "2.12.10"


lazy val root = (project in file("."))
  .settings(
    name := "sgit",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    libraryDependencies += "com.github.scopt" % "scopt_2.12" % "4.0.0-RC2",
    libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0"
  )

assemblyOption in assembly := (assemblyOption in assembly).value
  .copy(prependShellScript = Some(defaultUniversalScript(shebang = false)))

assemblyJarName in assembly := s"${name.value}"