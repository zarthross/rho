package org.http4s.rho

import fs2.Stream
import org.http4s.Method
import org.http4s.rho.bits.{Metadata, PathAST, SecurityScopesMetaData, TextMetaData}
import org.http4s.rho.swagger.models.Model
import shapeless.{HList, HNil}

import scala.reflect.runtime.universe._

package object swagger {

  /** Metadata carrier for specific routes */
  case class RouteDesc(msg: String) extends TextMetaData

  case class RouteTags(tags: List[String]) extends Metadata

  /** Scopes carrier for specific routes */
  case class RouteSecurityScope(definitions: Map[String, List[String]])
      extends SecurityScopesMetaData

  /** Add support for adding security scopes before a route using the ^^ operator */
  implicit class SecOps[F[_]](definitions: Map[String, List[String]]) {
    def ^^(method: Method): PathBuilder[F, HNil] = ^^(new PathBuilder[F, HNil](method, PathEmpty))

    def ^^(route: RhoRoute.Tpe[F]): PathBuilder[F, HNil] =
      new PathBuilder(route.method, PathAST.MetaCons(route.path, RouteSecurityScope(definitions)))

    def ^^[T <: HList](builder: PathBuilder[F, T]): PathBuilder[F, T] =
      new PathBuilder(
        builder.method,
        PathAST.MetaCons(builder.path, RouteSecurityScope(definitions)))
  }

  implicit class ReflectionHelpers[F[_]](t: Type) {
    import scala.reflect.runtime.universe._

    val genericStart = "«"
    val genericSep = ","
    val genericEnd = "»"

    def simpleName: String =
      t.typeSymbol.name.decodedName.toString + {
        if (t.typeArgs.isEmpty) ""
        else t.typeArgs.map(_.simpleName).mkString(genericStart, genericSep, genericEnd)
      }

    def fullName: String =
      t.typeSymbol.fullName + {
        if (t.typeArgs.isEmpty) ""
        else t.typeArgs.map(_.fullName).mkString(genericStart, genericSep, genericEnd)
      }

    def isArray: Boolean =
      t <:< typeOf[Array[_]]

    def isCollection: Boolean =
      t <:< typeOf[Array[_]] ||
        t <:< typeOf[Iterable[_]] ||
        t <:< typeOf[java.util.Collection[_]]

    def isEither: Boolean =
      t <:< typeOf[Either[_, _]]

    def isMap: Boolean =
      t <:< typeOf[collection.immutable.Map[_, _]] || t <:< typeOf[collection.Map[_, _]]

    def isNothingOrNull: Boolean =
      t <:< typeOf[Nothing] || t <:< typeOf[Null]

    def isOption: Boolean =
      t <:< typeOf[Option[_]]

    def isPrimitive: Boolean =
      Reflector.primitives.exists(_ =:= t)

    def isStream: Boolean =
      t <:< typeOf[Stream[G forSome { type G[_] }, _]]

    def isEffect(et: Type): Boolean =
      t <:< et

    def isUnitOrVoid: Boolean =
      t =:= typeOf[Unit] || t =:= typeOf[java.lang.Void]

    def isSwaggerFile: Boolean =
      t <:< typeOf[SwaggerFileResponse[_]]

    def isAnyVal: Boolean =
      t <:< typeOf[AnyVal]
  }

  val DefaultSwaggerFormats: SwaggerFormats = {
    val ignoreExistentialType: PartialFunction[Type, Set[Model]] = {
      case ExistentialType(_, _) => Set.empty
    }

    SwaggerFormats(
      ignoreExistentialType,
      SwaggerFormats.emptyFieldSerializers
    )
  }

  val EmptySwaggerFormats: SwaggerFormats =
    SwaggerFormats(SwaggerFormats.emptySerializers, SwaggerFormats.emptyFieldSerializers)

}
