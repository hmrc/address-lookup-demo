/*
 * Copyright 2016 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import sbt.Keys._
import sbt.Tests.{SubProcess, Group}
import sbt._
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin._
import uk.gov.hmrc.versioning.SbtGitVersioning


trait MicroService {

  import uk.gov.hmrc._
  import DefaultBuildSettings._
  import uk.gov.hmrc.{SbtBuildInfo, ShellPrompt}

  import TestPhases._

  val appName: String
//  val appVersion: String

  lazy val appDependencies : Seq[ModuleID] = ???
  lazy val plugins : Seq[Plugins] = Seq(play.PlayScala)
  lazy val playSettings : Seq[Setting[_]] = Seq.empty

  lazy val scoverageSettings = {
    import scoverage.ScoverageKeys
    Seq(
      // Semicolon-separated list of regexs matching classes to exclude
      ScoverageKeys.coverageExcludedPackages := "<empty>;Reverse.*;.*AuthService.*;models\\.data\\..*;views.html.*;uk.gov.hmrc.BuildInfo;app.*;prod.*;config.*;controllers.SessionCacheController",
      ScoverageKeys.coverageMinimum := 90,
      ScoverageKeys.coverageFailOnMinimum := false,
      ScoverageKeys.coverageHighlighting := true
    )
  }

  lazy val microservice = Project(appName, file("."))
    .enablePlugins(plugins : _*)
    .settings(playSettings : _*)
//    .settings(version := appVersion)
    .settings(scalaSettings: _*)
    .settings(scalaVersion := "2.11.7")
    .settings(publishingSettings: _*)
    .settings(defaultSettings(): _*)
    .settings(
      targetJvm := "jvm-1.8",
//      shellPrompt := ShellPrompt(appVersion),
      libraryDependencies ++= appDependencies,
      parallelExecution in Test := false,
      fork in Test := false,
      retrieveManaged := true
    )
    .settings(Repositories.playPublishingSettings : _*)
    .configs(IntegrationTest)
    .settings(inConfig(IntegrationTest)(Defaults.itSettings): _*)
//      .settings(coverageExcludedPackages  := """views\..*;controllers\..*Reverse.*;controllers\..*javascript.*;controllers\..*ref.*;Routes.*""")
    .settings(playSettings ++ scoverageSettings : _*)
    .settings(
      Keys.fork in IntegrationTest := false,
      unmanagedSourceDirectories in IntegrationTest <<= (baseDirectory in IntegrationTest)(base => Seq(base / "it")),
      addTestReportOption(IntegrationTest, "int-test-reports"),
      testGrouping in IntegrationTest := oneForkedJvmPerTest((definedTests in IntegrationTest).value),
      parallelExecution in IntegrationTest := false)
    .settings(SbtBuildInfo(): _*)
    .disablePlugins(sbt.plugins.JUnitXmlReportPlugin)
    .settings(resolvers += Resolver.bintrayRepo("hmrc", "releases"))
    .enablePlugins(SbtDistributablesPlugin, SbtGitVersioning)
}

private object TestPhases {

  def oneForkedJvmPerTest(tests: Seq[TestDefinition]) =
    tests map {
      test => new Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq("-Dtest.name=" + test.name))))
    }
}

private object Repositories {

  import uk.gov.hmrc._
  import PublishingSettings._

  lazy val playPublishingSettings : Seq[sbt.Setting[_]] = sbtrelease.ReleasePlugin.releaseSettings ++ Seq(

    credentials += SbtCredentials,

    publishArtifact in(Compile, packageDoc) := false,
    publishArtifact in(Compile, packageSrc) := false
  ) ++
    publishAllArtefacts
}
