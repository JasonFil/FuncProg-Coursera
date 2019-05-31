name := course.value + "-" + assignment.value

scalaVersion := "2.12.8"

scalacOptions ++= Seq("-deprecation")

// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % Test

// for funsets
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

// scalatest
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

// include the common dir
commonSourcePackages += "common"

courseId := "bRPXgjY9EeW6RApRXdjJPw"

