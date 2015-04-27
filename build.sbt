name := "staged-fold-fusion"

scalaVersion := "2.11.2"

scalaOrganization := "org.scala-lang.virtualized"

libraryDependencies ++= Seq(
  "EPFL" %% "lms" % "0.3-SNAPSHOT",
  //"org.scala-lang" % "scala-actors" % "2.10.0", // for ScalaTest
  "org.scalatest" % "scalatest_2.11" % "2.2.2"
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
