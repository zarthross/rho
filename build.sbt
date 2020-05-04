import sbt._
import Keys._
import spray.revolver.RevolverPlugin._

import com.typesafe.sbt.SbtGit.git

import scala.util.Properties.envOrNone

import Dependencies._, RhoPlugin._

lazy val rho = project
  .in(file("."))
  .disablePlugins(MimaPlugin)
  .settings(buildSettings)
  .aggregate(`rho-core`, `rho-hal`, `rho-swagger`, `rho-swagger-ui`, `rho-examples`)

lazy val `rho-core` = project
  .in(file("core"))
  .settings(mimaConfiguration)
  .settings(buildSettings)
  .settings(coreDeps)
  .settings(
    Compile / unmanagedSourceDirectories ++= {
      val baseDir = baseDirectory.value

      val mainSrcDir = "src/main/scala"
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, minor)) if minor <= 12 => Seq(baseDir / s"$mainSrcDir-2.12-")
        case Some((2, minor)) if minor >= 13 => Seq(baseDir / s"$mainSrcDir-2.13+")
        case _ => Nil
      }
    },
    libraryDependencies ++= Seq("org.scala-lang.modules" %% "scala-collection-compat" % "2.1.6")
  )

lazy val `rho-hal` = project
  .in(file("hal"))
  .settings(buildSettings)
  .settings(mimaConfiguration)
  .settings(halDeps)
  .dependsOn(`rho-core`)

lazy val `rho-swagger` = project
  .in(file("swagger"))
  .settings(buildSettings)
  .settings(mimaConfiguration)
  .settings(swaggerDeps)
  .dependsOn(`rho-core` % "compile->compile;test->test")

lazy val `rho-swagger-ui` = project
  .in(file("swagger-ui"))
  .settings(buildSettings)
  .settings(mimaConfiguration)
  .settings(swaggerUiDeps)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey]("swaggerUiVersion" -> Dependencies.swaggerUi.revision),
    buildInfoPackage := "org.http4s.rho.swagger.ui"
  )
  .dependsOn(`rho-swagger`)

lazy val docs = project
  .in(file("docs"))
  .settings(buildSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .enablePlugins(SiteScaladocPlugin)
  .enablePlugins(GhpagesPlugin)
  .settings(
    dontPublish,
    description := "Api Documentation",
    autoAPIMappings := true,
    scalacOptions in Compile := scaladocOptions(
      (baseDirectory in ThisBuild).value,
      version.value,
      apiVersion.value
    ),
    scalacOptions in (ScalaUnidoc, unidoc) ++= versionSpecificEnabledFlags(scalaVersion.value),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(
      `rho-core`,
      `rho-hal`,
      `rho-swagger`
    ),
    git.remoteRepo := "git@github.com:http4s/rho.git",
    ghpagesCleanSite := VersionedGhPages.cleanSite0.value,
    ghpagesSynchLocal := VersionedGhPages.synchLocal0.value,
    mappings in makeSite := {
      val (major, minor) = apiVersion.value
      for {
        (f, d) <- (mappings in (ScalaUnidoc, packageDoc)).value
      } yield (f, s"api/$major.$minor/$d")
    }
  )
  .dependsOn(`rho-core`, `rho-hal`, `rho-swagger`)

lazy val `rho-examples` = project
  .in(file("examples"))
  .disablePlugins(MimaPlugin)
  .settings(buildSettings)
  .settings(Revolver.settings)
  .settings(
    exampleDeps,
    dontPublish
  )
  .dependsOn(`rho-swagger`, `rho-swagger-ui`, `rho-hal`)

lazy val compilerFlags = Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
  "-Ywarn-unused",
  "-Xfatal-warnings"
)

def versionSpecificEnabledFlags(version: String) = (CrossVersion.partialVersion(version) match {
  case Some((2, 13)) => Seq.empty[String]
  case _ => Seq("-Ypartial-unification")
})

/* Don't publish setting */
lazy val dontPublish = packagedArtifacts := Map.empty

lazy val license = licenses in ThisBuild := Seq(
  "Apache License, Version 2.0" -> url(
    "http://www.apache.org/licenses/LICENSE-2.0.txt")
)

lazy val buildSettings = publishing ++
  Seq(
    scalaVersion := "2.13.1",
    crossScalaVersions := Seq(scalaVersion.value, "2.12.10"),
    scalacOptions := compilerFlags ++ versionSpecificEnabledFlags(scalaVersion.value),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    fork in run := true,
    organization in ThisBuild := "org.http4s",
    homepage in ThisBuild := Some(url(homepageUrl)),
    description := "A self documenting DSL build upon the http4s framework",
    license,
    libraryDependencies ++= specs2
  )

// to keep REPL usable
scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")

lazy val publishing = Seq(
  extras,
  credentials ++= travisCredentials.toSeq,
  publishMavenStyle in ThisBuild := true,
  publishArtifact in (ThisBuild, Test) := false,
  // Don't publish root pom.  It's not needed.
  packagedArtifacts in LocalRootProject := Map.empty,
  publishArtifact in Test := false,
  publishTo in ThisBuild := Some(nexusRepoFor(version.value)),
  scmInfo in ThisBuild := {
    val base = "github.com/http4s/rho"
    Some(
      ScmInfo(url(s"https://$base"),
              s"scm:git:https://$base",
              Some(s"scm:git:git@$base")))
  }
)

lazy val travisCredentials =
  (envOrNone("SONATYPE_USERNAME"), envOrNone("SONATYPE_PASSWORD")) match {
    case (Some(user), Some(pass)) =>
      Some(
        Credentials("Sonatype Nexus Repository Manager",
                    "oss.sonatype.org",
                    user,
                    pass))
    case _ =>
      None
  }

lazy val extras = pomExtra in ThisBuild := (
  <developers>
    <developer>
      <id>brycelane</id>
      <name>Bryce L. Anderson</name>
      <email>bryce.anderson22@gmail.com</email>
    </developer>
    <developer>
      <id>before</id>
      <name>André Rouél</name>
    </developer>
    <developer>
      <id>rossabaker</id>
      <name>Ross A. Baker</name>
      <email>ross@rossabaker.com</email>
    </developer>
    <developer>
      <id>zarthross</id>
      <name>Darren A Gibson</name>
      <email>zarthross@gmail.com</email>
    </developer>
  </developers>
)
