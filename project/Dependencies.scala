import sbt._
import Keys._
import explicitdeps.ExplicitDepsPlugin.autoImport._

object Dependencies {
  lazy val http4sVersion = "0.21.4"
  lazy val specs2Version = "4.9.4"

  lazy val http4sCore          = "org.http4s"                 %% "http4s-core"           % http4sVersion     
  lazy val http4sServer        = "org.http4s"                 %% "http4s-server"         % http4sVersion
  lazy val http4sDSL           = "org.http4s"                 %% "http4s-dsl"            % http4sVersion
  lazy val http4sBlaze         = "org.http4s"                 %% "http4s-blaze-server"   % http4sVersion
  lazy val http4sJetty         = "org.http4s"                 %% "http4s-servlet"        % http4sVersion
  lazy val http4sJson4sJackson = "org.http4s"                 %% "http4s-json4s-jackson" % http4sVersion
  lazy val http4sXmlInstances  = "org.http4s"                 %% "http4s-scala-xml"      % http4sVersion
  lazy val json4sCore          = "org.json4s"                 %% "json4s-core"           % "3.6.7" 
  lazy val json4sAST           = "org.json4s"                 %% "json4s-ast"            % "3.6.7"    
  lazy val json4s              = "org.json4s"                 %% "json4s-ext"            % "3.6.7"
  lazy val json4sJackson       = "org.json4s"                 %% "json4s-jackson"        % json4s.revision
  lazy val swaggerModels       = "io.swagger"                  % "swagger-models"        % "1.6.1"
  lazy val swaggerCore         = "io.swagger"                  % "swagger-core"          % swaggerModels.revision
  lazy val logbackClassic      = "ch.qos.logback"              % "logback-classic"       % "1.2.3"
  lazy val uadetector          = "net.sf.uadetector"           % "uadetector-resources"  % "2014.10"
  lazy val shapeless           = "com.chuusai"                %% "shapeless"             % "2.3.3"
  lazy val scalaXml            = "org.scala-lang.modules"     %% "scala-xml"             % "1.3.0"
  lazy val swaggerUi           = "org.webjars"                 % "swagger-ui"            % "3.25.1"

  lazy val specs2              = Seq("org.specs2"              %% "specs2-core"          % specs2Version % "test",
                                     "org.specs2"              %% "specs2-scalacheck"    % specs2Version % "test" )

  lazy val `scala-reflect`     = "org.scala-lang"              % "scala-reflect"

  lazy val coreDeps = libraryDependencies ++= Seq(
    `scala-reflect` % scalaVersion.value,
    http4sCore,
    shapeless,
    http4sServer % "test",
    logbackClassic % "test"
  )

  lazy val halDeps = libraryDependencies ++= Seq(
    json4sCore, 
    json4sAST,
    json4sJackson % "test"
  )

  lazy val swaggerDeps = libraryDependencies ++= Seq(
    scalaXml,
    swaggerCore,
    swaggerModels,

    json4s % "test",
    json4sJackson % "test"
  )

  lazy val swaggerUiDeps = Seq(
    libraryDependencies ++= Seq(swaggerUi),
    unusedCompileDependenciesFilter -= moduleFilter("org.webjars", "swagger-ui")
  )

  lazy val exampleDeps = libraryDependencies ++= Seq(
    http4sServer,
    http4sBlaze,
    http4sDSL,
    json4s,
    json4sJackson,
    uadetector
  )
}
