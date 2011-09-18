import sbt._

import Keys._
import AndroidKeys._

object General {
  val settings = Defaults.defaultSettings ++ Seq (
    organization := "org.positronicnet",
    version := "0.3-SNAPSHOT",
    scalaVersion := "2.9.0-1",
    platformName in Android := "android-7"
  )

  lazy val fullAndroidSettings =
    General.settings ++
    AndroidProject.androidSettings ++
    TypedResources.settings ++
    AndroidMarketPublish.settings ++ Seq (
      keyalias in Android := "change-me",
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"
    )
}

object AndroidBuild extends Build {

  lazy val libproj = Project (
    "PositronicNetLib",
    file("."),

    // Test setup --- include Robolectric, tell it where to find
    // resources and manifest, and don't try to run tests in parallel
    // (lest the DB tests stomp all over each other).

    settings = General.fullAndroidSettings ++ Seq(

      parallelExecution in Test := false,
      libraryDependencies += "com.pivotallabs"%"robolectric"%"1.0-RC1" % "test",

      testOptions in Test ++= Seq(
        Tests.Argument("-DandroidResPath=src/main/res"),
        Tests.Argument("-DandroidManifestPath=src/main/AndroidManifest.xml"))
    )
  ) dependsOn ( roboScalaTest % "test->compile" )

  // Separate packaging for the glue code to get Robolectric support
  // in a ScalaTest suite.  This shows up as a trait named RobolectricTests
  // in package org.positronicnet.test, which extends org.scalatest.Suite.
  // Can be published as "roboscalatest".

  lazy val roboScalaTest = Project (
    "RoboScalaTest",
    file("testsupport"),
    settings = General.settings ++ AndroidProject.androidSettings ++ Seq (
      keyalias in Android := "change-me",
      libraryDependencies ++= Seq( 
        "org.scalatest" %% "scalatest" % "1.6.1",
        "com.pivotallabs"%"robolectric"%"1.0-RC1"
      )))

  def sampleProject( name: String, dir: String ) =
    Project( name, file( "sample/"+dir ), 
             settings = General.fullAndroidSettings 
           ) dependsOn (libproj % "compile")

  lazy val todo     = sampleProject( "SampleTodo",    "todo_app" )
  lazy val call_log = sampleProject( "SampleCallLog", "call_log_app" )
}
