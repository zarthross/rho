package org.http4s.rho.swagger

import com.github.fge.jsonschema.core.report.{ProcessingMessage, ProcessingReport}
import com.github.fge.jsonschema.main.JsonSchemaFactory
import org.specs2.mutable.Specification
import io.swagger.util.Json
import org.http4s.rho.{RhoRoute, RhoService}
import org.specs2.matcher.Matcher

import scala.collection.JavaConverters._

class SwaggerValidSpec extends Specification {
  import SwaggerValidSpec._
  import org.http4s.rho._
  import org.http4s.Method._
  import org.http4s.rho.bits.ResponseGeneratorInstances._
  implicit def defaultCompiler: CompileService[RhoRoute.Tpe] = CompileService.identityCompiler

  "Generated Swagger.json is valid" in {
    Seq(
      GET / "foo" / pathVar[Int] |>> { (i: Int) => Ok("") },
      GET / "foo" / pathVar[String] |>> { (i: String) => "" }
    ).parseSwagger must beValid
  }
}
object SwaggerValidSpec extends Specification {
  lazy val schema = JsonSchemaFactory.byDefault.getJsonSchema(
    Json.mapper.readTree(scala.io.Source.fromResource("swagger-v2-schema.json").mkString)
  )

  val beValid: Matcher[ProcessingReport] = { s: ProcessingReport =>
    val message = s.asScala.mkString("\n")
    (s.isSuccess, "Generated Swagger is not valid.\n" + message)
  }

  def validUsingJsonSchema(swaggerToValidate: String): ProcessingReport = {
    val specObject = Json.mapper.readTree(swaggerToValidate)
    val report = schema.validate(specObject)
    report
  }

  def routesToSwaggerJson(rhoRoutes: Seq[RhoRoute.Tpe]) = {
    val swagger = SwaggerSupport.createSwagger()(rhoRoutes)
    val json = Json.mapper()
      .writerWithDefaultPrettyPrinter()
      .writeValueAsString(swagger.toJModel)
    //println(json)
    validUsingJsonSchema(json)
  }

  implicit class RhoSwaggerJson(val rhoService: RhoService) extends AnyVal {
    def parseSwagger() = routesToSwaggerJson(rhoService.getRoutes)
  }

  implicit class RhoRouteJson(val rhoRoute: RhoRoute.Tpe) extends AnyVal {
    def parseSwagger() = routesToSwaggerJson(rhoRoute :: Nil)
  }

  implicit class RhoRoutesJson(val rhoRoutes: Seq[RhoRoute.Tpe]) extends AnyVal {
    def parseSwagger() = routesToSwaggerJson(rhoRoutes)
  }
}