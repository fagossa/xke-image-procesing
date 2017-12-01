import sbt.Keys.testOptions

lazy val root = (project in file("."))
  .settings(
    name := "scala-front-propagation",
    organization := "fr.xebia.scala",
    scalaVersion := "2.12.3",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.0.0-RC1",
      "org.scalatest" %% "scalatest" % "3.0.4" % "test"
    )
  )
  .settings(
    scalafmtVersion := "1.3.0",
    scalafmtOnCompile in ThisBuild := true,
    scalacOptions in (Compile, doc) := Seq(
      "-encoding",
      "UTF-8",
      "-target:jvm-1.8",
      "-unchecked",
      "-deprecation",
      "-groups",
      "-Xlint:_",
      "-Xfatal-warnings",
      "-Ywarn-dead-code",
      "-Ywarn-unused",
      "-Yconst-opt",
      "-Yclosure-elim",
      "-Ydead-code",
      "-implicits"
    ),
    javaOptions += "-Xmx512M",
    testOptions in Test += Tests.Argument("-oD"),
    fork in Runtime := true
  )
