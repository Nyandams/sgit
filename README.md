# Sgit [![Build Status](https://travis-ci.com/Nyandams/sgit.svg?branch=master)](https://travis-ci.com/Nyandams/sgit)
Sgit is a simple git cli in Scala

SonarCloud of the project: [sgit_sonarcloud](https://sonarcloud.io/dashboard?id=Nyandams_sgit)


Here is a list of the current commands implemented:
* sgit init
* sgit add \<file>...
* sgit rm \<file>...
* sgit commit -m \<message>
* sgit status
* sgit branch
    * -v
    * \<branch name>
* sgit tag
    * \<name tag>
* sgit diff
* sgit log
* sgit checkout \<name of branch/tag or commit>

## Installation of Sgit
```bash
sbt assembly
```
then add sgit/target/scala-2.12 to your path