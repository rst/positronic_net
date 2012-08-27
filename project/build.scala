import sbt._

import Keys._
import AndroidKeys._

object General {
  val settings = Defaults.defaultSettings ++ Seq (
    organization := "org.positronicnet",
    version := "0.4-SNAPSHOT",
    scalaVersion := "2.9.0-1",
    platformName in Android := "android-14"
  )

  lazy val fullAndroidSettings =
    General.settings ++
    AndroidProject.androidSettings ++
    TypedResources.settings ++
    AndroidMarketPublish.settings ++ Seq (
      keyalias in Android := "change-me",
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test",
      proguardOption in Android := """
       -keepclassmembers class * implements java.io.Serializable {
        private static final java.io.ObjectStreamField[] serialPersistentFields;
        private void writeObject(java.io.ObjectOutputStream);
        private void readObject(java.io.ObjectInputStream);
        java.lang.Object writeReplace();
        java.lang.Object readResolve();
       }
      """
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
      libraryDependencies += "com.pivotallabs"%"robolectric"%"1.1" % "test",

      testOptions in Test ++= Seq(
        Tests.Argument("-DandroidResPath=src/main/res"),
        Tests.Argument("-DandroidManifestPath=src/main/AndroidManifest.xml"))
    )
  ) dependsOn ( roboScalaTest % "test" )

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
        "com.pivotallabs"%"robolectric"%"1.1"
      )))

  // Bundled sample projects

  def sampleProject( name: String, dir: String ) = {
    val projSrc = "sample/" + dir + "/src/main"
    Project( name, file("sample")/dir, 
             settings = General.fullAndroidSettings ++ (
               testOptions in Test ++= Seq(
                 Tests.Argument("-DandroidResPath=" + projSrc + "/res"),
                 Tests.Argument("-DandroidManifestPath=" + projSrc + 
                                "/AndroidManifest.xml"))
             ))
      .dependsOn (libproj % "compile")
      .dependsOn (roboScalaTest % "test")
  }

  lazy val todo     = sampleProject( "SampleTodo",     "todo_app" )
  lazy val todocp   = sampleProject( "SampleTodoCp",   "todo_app_cp" )
  lazy val call_log = sampleProject( "SampleCallLog",  "call_log_app" )
  lazy val contacts = sampleProject( "SampleContacts", "contacts_app" )
}
