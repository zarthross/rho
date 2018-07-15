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
  def name: String
}
final case class ListResultMetadata[L[_], T](items: ResultMetadata[T]) extends ResultMetadata[L[T]] {
  def name: String = s"List[${items.name}]"
}
final case class OptionResultMetadata[T](item: ResultMetadata[T]) extends ResultPrimitiveMetadata[Option[T]] {
  def name: String = s"Option[${item.name}]"
}
final case class ObjectResultMetadata[T](name: String, properties: Map[String, ResultMetadata.Tpe]) extends ResultMetadata[T]


object ResultPrimitiveMetadata {
  type Tpe = ResultPrimitiveMetadata[_]

  def apply[T](implicit resultMetadata: ResultPrimitiveMetadata[T]): ResultPrimitiveMetadata[T] = resultMetadata
}
trait ResultPrimitiveMetadata[T] extends ResultMetadata[T] {

}
trait ResultPrimitiveMetadataInstances {
  @volatile implicit lazy val simpleStringResultMetadata: ResultPrimitiveMetadata[String] = new ResultPrimitiveMetadata[String] {
    val name: String = "String"
  }

  protected def instance[T](name: String): ResultPrimitiveMetadata[T] = {
    val nme = name
    new ResultPrimitiveMetadata[T] {
      val name: String = nme
    }
  }

  @volatile implicit lazy val byteResultMetadata: ResultPrimitiveMetadata[Byte] = instance[Byte]("Byte")
  @volatile implicit lazy val unitResultMetadata: ResultPrimitiveMetadata[Unit] = instance[Unit]("Unit")
  @volatile implicit lazy val BooleanResultMetadata: ResultPrimitiveMetadata[Boolean] = instance[Boolean]("Boolean")
  @volatile implicit lazy val DoubleResultMetadata: ResultPrimitiveMetadata[Double] = instance[Double]("Double")
  @volatile implicit lazy val FloatResultMetadata: ResultPrimitiveMetadata[Float] = instance[Float]("Float")
  @volatile implicit lazy val IntResultMetadata: ResultPrimitiveMetadata[Int] = instance[Int]("Int")
  @volatile implicit lazy val LongResultMetadata: ResultPrimitiveMetadata[Long] = instance[Long]("Long")
  @volatile implicit lazy val ShortResultMetadata: ResultPrimitiveMetadata[Short] = instance[Short]("Short")

  @volatile implicit lazy val UUIDResultMetadata: ResultPrimitiveMetadata[UUID] = instance[UUID]("UUID")
  @volatile implicit lazy val DateResultMetadata: ResultPrimitiveMetadata[Date] = instance[Date]("Date")
  @volatile implicit lazy val InstantResultMetadata: ResultPrimitiveMetadata[Instant] = instance[Instant]("Instant")
}

object ResultMetadata extends ResultMetaDataInstances with ResultPrimitiveMetadataInstances with ResultMetadataLowPriority {
  type Tpe = ResultMetadata[_]

  def apply[T](implicit resultMetadata: ResultMetadata[T]): ResultMetadata[T] = resultMetadata
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
    val name = typetag.tpe.typeSymbol.name.toString

    override def hashCode(): Int = name.hashCode

    override def equals(o: scala.Any): Boolean = o match {
      case r: ResultMetadata[_] => r.name == name
      case _ => false
    }
  }
}