import sbt._
import sbt.Keys._

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object Dependencies {
}

object Settings {
  type PE = Project => Project

  def common: PE =
    _.enablePlugins(ScalaJSPlugin)
      .settings(
        organization := "org.github.nigredo",
        resolvers ++= Seq(
          Resolver.sonatypeRepo("releases"),
          Resolver.sonatypeRepo("snapshots")
        ),
        libraryDependencies ++= Seq(
          "com.chuusai" %%% "shapeless" % "2.3.2",
          "org.scalaz" %%% "scalaz-core" % "7.2.9"
        )
    )

  def scalatest: PE =
    _.settings(
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
    )
}

object JsRecordBuild {

  lazy val core = Project(
    id = "core",
    base = file(".")
  ).settings(
    name := "core"
  ).configure(Settings.common, Settings.scalatest)
}
