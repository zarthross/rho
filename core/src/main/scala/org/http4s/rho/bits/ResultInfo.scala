package org.http4s.rho.bits

import java.time.Instant
import java.util.{Date, UUID}

import cats.effect.Sync
import org.http4s.Status

import scala.collection.Seq

/** Information about the response type */
sealed trait ResultInfo

case class TypeOnly(tpe: ResultMetadata.Tpe) extends ResultInfo

case class StatusAndType(status: Status, tpe: ResultMetadata.Tpe) extends ResultInfo

case class StatusOnly(status: Status) extends ResultInfo


sealed trait ResultMetadata[T] {
  val name: String = "TEST"
}
final case class ListResultMetadata[L[_], T](items: ResultMetadata[T]) extends ResultMetadata[L[T]]
final case class ObjectResultMetadata[T](properties: Map[String, ResultMetadata.Tpe]) extends ResultMetadata[T]

object ResultPrimitiveMetadata {
  type Tpe = ResultPrimitiveMetadata[_]

  def apply[T](implicit resultMetadata: ResultPrimitiveMetadata[T]): ResultPrimitiveMetadata[T] = resultMetadata
}
trait ResultPrimitiveMetadata[T] extends ResultMetadata[T] {

}
final case class StringResultMetadata() extends ResultPrimitiveMetadata[String]

object ResultMetadata {
  type Tpe = ResultMetadata[_]

  def apply[T](implicit resultMetadata: ResultMetadata[T]): ResultMetadata[T] = resultMetadata

  implicit def resultMetadata[T]: ResultMetadata[T] = new ResultMetadata[T] {}

  implicit def listResultMetadata[T: ResultMetadata, L[_] <: Seq[_]]: ResultMetadata[L[T]] = ListResultMetadata[L, T](ResultMetadata[T])

  implicit def syncResultData[T: ResultMetadata, F[_]: Sync]: ResultMetadata[F[T]] =
    ResultMetadata[T].asInstanceOf[ResultMetadata[F[T]]]

  @volatile lazy val simpleStringResultMetadata: StringResultMetadata = StringResultMetadata()
  @volatile implicit lazy val byteResultMetadata: ResultPrimitiveMetadata[Byte] = new ResultPrimitiveMetadata[Byte] {}
  @volatile implicit lazy val unitResultMetadata: ResultPrimitiveMetadata[Unit] = new ResultPrimitiveMetadata[Unit] {}

  @volatile implicit lazy val BooleanResultMetadata: ResultPrimitiveMetadata[Boolean] = new ResultPrimitiveMetadata[Boolean] {}

  @volatile implicit lazy val DoubleResultMetadata: ResultPrimitiveMetadata[Double] = new ResultPrimitiveMetadata[Double] {}
  @volatile implicit lazy val FloatResultMetadata: ResultPrimitiveMetadata[Float] = new ResultPrimitiveMetadata[Float] {}
  @volatile implicit lazy val IntResultMetadata: ResultPrimitiveMetadata[Int] = new ResultPrimitiveMetadata[Int] {}
  @volatile implicit lazy val LongResultMetadata: ResultPrimitiveMetadata[Long] = new ResultPrimitiveMetadata[Long] {}
  @volatile implicit lazy val ShortResultMetadata: ResultPrimitiveMetadata[Short] = new ResultPrimitiveMetadata[Short] {}
  @volatile implicit lazy val UUIDResultMetadata: ResultPrimitiveMetadata[UUID] = new ResultPrimitiveMetadata[UUID] {}

  @volatile implicit lazy val DateResultMetadata: ResultPrimitiveMetadata[Date] = new ResultPrimitiveMetadata[Date] {}
  @volatile implicit lazy val InstanceResultMetadata: ResultPrimitiveMetadata[Instant] = new ResultPrimitiveMetadata[Instant] {}

}