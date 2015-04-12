name := "staged-fold-fusion"

scalaVersion := "2.10.2"

scalaOrganization := "org.scala-lang.virtualized"

libraryDependencies ++= Seq(
  "EPFL" %% "lms" % "0.3-SNAPSHOT",
  //"org.scala-lang" % "scala-actors" % "2.10.0", // for ScalaTest
  "org.scalatest" % "scalatest_2.10" % "2.1.5" % "test"
)

scalacOptions ++= Seq(
  "-Yvirtualize"
  //"-P:continuations:enable"//,
  //"-optimize"//,
  //"-deprecation",
  //"-feature",
  //"-Yinline-warnings"
)

defaultScalariformSettings
