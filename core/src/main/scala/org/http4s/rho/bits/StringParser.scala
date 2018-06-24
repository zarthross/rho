package org.http4s
package rho.bits

import java.text.SimpleDateFormat
import java.time.Instant
import java.util.{Date, UUID}

import cats.Monad

import scala.util.control.NonFatal

/** Parse values from a `String`
  *
  * @tparam T The result type generated by the parser.
  */
trait StringParser[F[_], T] extends ResponseGeneratorInstances[F] {

  /** Attempt to parse the `String`. */
  def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, T]

  /** TypeTag of the type T */
  def metadata: ResultPrimitiveMetadata[T]

  def invalidNumberFormat[A](n : String)(implicit F: Monad[F]): FailureResponse[F] = FailureResponse.pure[F] {
    BadRequest.pure(s"Invalid number format: '$n'")
  }
}

class BooleanParser[F[_]] extends StringParser[F, Boolean] {
  override val metadata: ResultPrimitiveMetadata[Boolean] = ResultPrimitiveMetadata[Boolean]

  override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, Boolean] = s match {
    case "true"  => SuccessResponse(true)
    case "false" => SuccessResponse(false)
    case _       => FailureResponse.pure[F] { BadRequest.pure(s"Invalid boolean format: '$s'") }
  }
}

class DoubleParser[F[_]] extends StringParser[F, Double] {
  override val metadata: ResultPrimitiveMetadata[Double] = ResultPrimitiveMetadata[Double]

  override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, Double] =
    try SuccessResponse(s.toDouble)
    catch { case _: NumberFormatException => invalidNumberFormat[Double](s) }
}

class FloatParser[F[_]] extends StringParser[F, Float] {
  override val metadata: ResultPrimitiveMetadata[Float] = ResultPrimitiveMetadata[Float]

  override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, Float] =
    try SuccessResponse(s.toFloat)
    catch { case _: NumberFormatException => invalidNumberFormat[Float](s) }
}

class IntParser[F[_]] extends StringParser[F, Int] {
  override val metadata: ResultPrimitiveMetadata[Int] = ResultPrimitiveMetadata[Int]

  override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, Int] =
    try SuccessResponse(s.toInt)
    catch { case _: NumberFormatException => invalidNumberFormat[Int](s) }
}

class LongParser[F[_]] extends StringParser[F, Long] {
  override val metadata: ResultPrimitiveMetadata[Long] = ResultPrimitiveMetadata[Long]

  override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, Long] =
    try SuccessResponse(s.toLong)
    catch { case _: NumberFormatException => invalidNumberFormat[Long](s) }
}

class ShortParser[F[_]] extends StringParser[F, Short] {
  override val metadata: ResultPrimitiveMetadata[Short] = ResultPrimitiveMetadata[Short]

  override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, Short] =
    try SuccessResponse(s.toShort)
    catch { case _: NumberFormatException => invalidNumberFormat[Short](s) }
}

class DateParser[F[_]] extends StringParser[F, Date] {
  override val metadata: ResultPrimitiveMetadata[Date] = ResultPrimitiveMetadata[Date]

  override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, Date] =
    try {
      val df = new SimpleDateFormat("yyyy-MM-dd")
      SuccessResponse(df.parse(s))
    } catch {
      case NonFatal(_) =>
        FailureResponse.pure[F] {
          BadRequest.pure(s"Invalid date format, should be 'yyyy-MM-dd': $s")
        }
    }
}

class InstantParser[F[_]] extends StringParser[F, Instant] {
  override val metadata: ResultPrimitiveMetadata[Instant] = ResultPrimitiveMetadata[Instant]

  override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, Instant] =
    try {
      SuccessResponse(Instant.parse(s))
    } catch {
      case NonFatal(_) =>
        FailureResponse.pure[F] {
          BadRequest.pure(s"Invalid instant format, should be in 'yyyy-MM-ddThh:mm:ssZ' format: $s")
        }
    }
}

class UUIDParser[F[_]] extends StringParser[F, UUID] {
  override val metadata: ResultPrimitiveMetadata[UUID] = ResultPrimitiveMetadata[UUID]

  override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, UUID] =
    try SuccessResponse(UUID.fromString(s))
    catch {
      case NonFatal(_) =>
        FailureResponse.pure[F] {
          BadRequest.pure(s"Invalid uuid format: $s")
        }
    }
}

object StringParser {

  ////////////////////// Default parsers //////////////////////////////

  implicit def booleanParser[F[_]]: BooleanParser[F] = new BooleanParser[F]()
  implicit def doubleParser[F[_]]: DoubleParser[F] = new DoubleParser[F]()
  implicit def floatParser[F[_]]: FloatParser[F] = new FloatParser[F]()
  implicit def intParser[F[_]]: IntParser[F] = new IntParser[F]()
  implicit def longParser[F[_]]: LongParser[F] = new LongParser[F]()
  implicit def shortParser[F[_]]: ShortParser[F] = new ShortParser[F]()
  implicit def dateParser[F[_]]: DateParser[F] = new DateParser[F]()
  implicit def instantParser[F[_]]: InstantParser[F] = new InstantParser[F]()
  implicit def uuidParser[F[_]]: UUIDParser[F] = new UUIDParser[F]()

  implicit def strParser[F[_]]: StringParser[F, String] = new StringParser[F, String] {

    override val metadata: ResultPrimitiveMetadata[String] = ResultMetadata.simpleStringResultMetadata

    override def parse(s: String)(implicit F: Monad[F]): ResultResponse[F, String] =
      SuccessResponse(s)
  }
}
