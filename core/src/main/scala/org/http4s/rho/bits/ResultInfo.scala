package org.http4s.rho.bits

import java.time.Instant
import java.util.{Date, UUID}

import scala.collection.Seq

import org.http4s.Status


/** Information about the response type */
sealed trait ResultInfo

case class TypeOnly(tpe: ResultMetadata.Tpe) extends ResultInfo

case class StatusAndType(status: Status, tpe: ResultMetadata.Tpe) extends ResultInfo

case class StatusOnly(status: Status) extends ResultInfo


sealed trait ResultMetadata[T] {
  def simpleName: String
  def fullName: String = ""

  def isCollection: Boolean = false
  def isOptional: Boolean = false
}
final case class ListResultMetadata[L[_], T](items: ResultMetadata[T]) extends ResultMetadata[L[T]] {
  def simpleName: String = s"List[${items.simpleName}]"

  override val isCollection: Boolean = true
}
final case class OptionResultMetadata[T](item: ResultMetadata[T]) extends ResultPrimitiveMetadata[Option[T]] {
  def simpleName: String = s"Option[${item.simpleName}]"

  override val isOptional: Boolean = true
}
final case class ObjectResultMetadata[T](simpleName: String, properties: Map[String, ResultMetadata.Tpe]) extends ResultMetadata[T]


object ResultPrimitiveMetadata {
  type Tpe = ResultPrimitiveMetadata[_]

  def apply[T](implicit resultMetadata: ResultPrimitiveMetadata[T]): ResultPrimitiveMetadata[T] = resultMetadata

  def from[F](implicit resultMetadata: ResultPrimitiveMetadata[F]): {
    def to[T]: ResultPrimitiveMetadata[T] // TODO: Clean this up.
  } = new {
    def to[T]: ResultPrimitiveMetadata[T] = resultMetadata.asInstanceOf[ResultPrimitiveMetadata[T]]
  }
}
trait ResultPrimitiveMetadata[T] extends ResultMetadata[T] {

}
trait ResultPrimitiveMetadataInstances {
  @volatile implicit lazy val simpleStringResultMetadata: ResultPrimitiveMetadata[String] = new ResultPrimitiveMetadata[String] {
    val simpleName: String = "string"
  }

  protected def instance[T](name: String): ResultPrimitiveMetadata[T] = {
    val nme = name
    new ResultPrimitiveMetadata[T] {
      val simpleName: String = nme
    }
  }

  @volatile implicit lazy val unitResultMetadata: ResultPrimitiveMetadata[Unit] = instance[Unit]("Unit")
  @volatile implicit lazy val byteResultMetadata: ResultPrimitiveMetadata[Byte] = instance[Byte]("Byte")
  @volatile implicit lazy val BooleanResultMetadata: ResultPrimitiveMetadata[Boolean] = instance[Boolean]("Boolean")
  @volatile implicit lazy val DoubleResultMetadata: ResultPrimitiveMetadata[Double] = instance[Double]("Double")
  @volatile implicit lazy val FloatResultMetadata: ResultPrimitiveMetadata[Float] = instance[Float]("Float")
  @volatile implicit lazy val IntResultMetadata: ResultPrimitiveMetadata[Int] = instance[Int]("integer")
  @volatile implicit lazy val LongResultMetadata: ResultPrimitiveMetadata[Long] = instance[Long]("Long")
  @volatile implicit lazy val ShortResultMetadata: ResultPrimitiveMetadata[Short] = instance[Short]("Short")

  @volatile implicit lazy val UUIDResultMetadata: ResultPrimitiveMetadata[UUID] = instance[UUID]("UUID")
  @volatile implicit lazy val DateResultMetadata: ResultPrimitiveMetadata[Date] = instance[Date]("Date")
  @volatile implicit lazy val InstantResultMetadata: ResultPrimitiveMetadata[Instant] = instance[Instant]("Instant")
}

object ResultMetadata extends ResultMetaDataInstances with ResultPrimitiveMetadataInstances with ResultMetadataLowPriority {
  type Tpe = ResultMetadata[_]

  def apply[T](implicit resultMetadata: ResultMetadata[T]): ResultMetadata[T] = resultMetadata

  def from[F](implicit resultMetadata: ResultMetadata[F]): {
    def to[T]: ResultMetadata[T]
  } = new {
    def to[T]: ResultMetadata[T] = resultMetadata.asInstanceOf[ResultMetadata[T]]
  }
}

trait ResultMetaDataInstances {
  implicit def listResultMetadata[T: ResultMetadata, L[_] <: Seq[_]]: ResultMetadata[L[T]] = ListResultMetadata[L, T](ResultMetadata[T])
  implicit def optionResultMetadata[T: ResultMetadata]: ResultPrimitiveMetadata[Option[T]] = OptionResultMetadata[T](ResultMetadata[T])

  /*import cats.effect.Sync
  implicit def syncResultData[T: ResultMetadata, F[_]: Sync]: ResultMetadata[F[T]] =
    ResultMetadata[T].asInstanceOf[ResultMetadata[F[T]]]*/
}

trait ResultMetadataLowPriority {
  // TODO: Remove after getting basic setup done.
  // This just removes errors because the type class isn't defined
  import scala.reflect.runtime.universe.WeakTypeTag
  implicit def generic[T](implicit typetag: WeakTypeTag[T]): ResultPrimitiveMetadata[T] = new ResultPrimitiveMetadata[T] {
    val simpleName = typetag.tpe.typeSymbol.name.toString

    override def hashCode(): Int = simpleName.hashCode

    override def equals(o: scala.Any): Boolean = o match {
      case r: ResultMetadata[_] => r.simpleName == simpleName
      case _ => false
    }
  }
}