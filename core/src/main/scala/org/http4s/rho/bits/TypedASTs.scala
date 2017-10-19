package org.http4s
package rho.bits

import org.http4s.rho.UriConvertible
import org.http4s.rho.bits.RequestAST._
import shapeless.ops.function.FnToProduct
import shapeless.{HList, HNil}
import shapeless.ops.hlist.{Prepend, Reverse}



/** Typed shell for the Header operations of the DSL */
final case class TypedHeader[F[_], T <: HList](rule: RequestRule[F]) {

  /** Match this rule or `alt` rule if this rule fails
    *
    * Alias of `||`
    */
  def or(alt: TypedHeader[F, T]): TypedHeader[F, T] = TypedHeader(OrRule(this.rule, alt.rule))

  /** Match this rule or `alt` rule if this rule fails
    *
    * Alias of `or`
    */
  def ||(alt: TypedHeader[F, T]): TypedHeader[F, T] = or(alt)

  /** Match this rule and then `next` rule
    *
    * Alias of `&&`
    */
  def and[T1 <: HList](next: TypedHeader[F, T1])(implicit prepend: Prepend[T1, T]): TypedHeader[F, prepend.Out] =
    TypedHeader(AndRule(this.rule, next.rule))

  /** Match this rule and then `next` rule
    *
    * Alias of `and`
    */
  def &&[T1 <: HList](next: TypedHeader[F, T1])(implicit prepend: Prepend[T1, T]): TypedHeader[F, prepend.Out] = and(next)

  /** Enclose this rule in metadata
    *
    * Type safe way to add metadata to a [[TypedHeader]] rule
    */
  def withMetadata(metadata: Metadata): TypedHeader[F, T] = copy(MetaRule(rule, metadata))

  /** Ignore the output generated by this rule
    *
    * The new rule will perform all the same actions and actually compute the results but they
    * will be ignored during the route evaluation
    */
  def ignore: TypedHeader[F, HNil] = copy(IgnoreRule(rule))

  /** Map the output generated by this rule into a new type
    *
    * The new rule will have all the original metadata.
    */
  def map[TR <: HList, FU, R](f: FU)(implicit rev: Reverse.Aux[T, TR], fp: FnToProduct.Aux[FU, TR => R]): TypedHeader[F, shapeless.::[R, HNil]] = {
    import shapeless.::
    val fn: T => R :: HNil = t => fp(f)(t.reverse) :: HNil
    TypedHeader(MapRule[F, T, R :: HNil](this.rule, fn))
  }
}

/** Typed shell for the Query operations of the DSL */
final case class TypedQuery[F[_], T <: HList](rule: RequestRule[F]) extends UriConvertible[F] {

  /** Match this rule or `alt` rule if this rule fails
    *
    * Alias of `||`
    */
  def or(alt: TypedQuery[F, T]): TypedQuery[F, T] = TypedQuery(OrRule(this.rule, alt.rule))

  /** Match this rule or `alt` rule if this rule fails
    *
    * Alias of `or`
    */
  def ||(alt: TypedQuery[F, T]): TypedQuery[F, T] = or(alt)

  /** Match this rule and then `next` rule
    *
    * Alias of `&&`
    */
  def and[T1 <: HList](next: TypedQuery[F, T1])(implicit prepend: Prepend[T1, T]): TypedQuery[F, prepend.Out] =
    TypedQuery(AndRule(this.rule, next.rule))

  /** Match this rule and then `next` rule
    *
    * Alias of `and`
    */
  def &&[T1 <: HList](next: TypedQuery[F, T1])(implicit prepend: Prepend[T1, T]): TypedQuery[F, prepend.Out] = and(next)

  /** Match this rule and then `next` rule
    *
    * Alias of `and`
    */
  def &[T1 <: HList](next: TypedQuery[F, T1])(implicit prepend: Prepend[T1, T]): TypedQuery[F, prepend.Out] = and(next)

  /** Enclose this rule in metadata
    *
    * Type safe way to add metadata to a [[TypedQuery]] rule
    */
  def withMetadata(metadata: Metadata): TypedQuery[F, T] = copy(MetaRule(rule, metadata))

  /** Ignore the output generated by this rule
    *
    * The new rule will perform all the same actions and actually compute the results but they
    * will be ignored during the route evaluation
    */
  def ignore: TypedQuery[F, HNil] = copy(IgnoreRule(rule))

  /** Map the output generated by this rule into a new type
    *
    * The new rule will have all the original metadata.
    */
  def map[TR <: HList, FU, R](f: FU)(implicit rev: Reverse.Aux[T, TR], fp: FnToProduct.Aux[FU, TR => R]): TypedQuery[F, shapeless.::[R, HNil]] = {
    import shapeless.::
    val fn: T => R :: HNil = t => fp(f)(t.reverse) :: HNil
    TypedQuery(MapRule[F, T, R :: HNil](this.rule, fn))
  }


  /**
   * Resolves names of query parameters to capture
   */
  val names: List[String] = {
    @scala.annotation.tailrec
    def go(rule: List[RequestRule[F]], acc: List[String]): List[String] = rule match {
      case Nil => acc
      case MetaRule(q, QueryMetaData(n,_,_,_,_)) :: rs  => go(q :: rs, n :: acc)
      case MetaRule(q, _) :: rs                         => go(q :: rs, acc)
      case AndRule(a, b) :: rs                          => go(a :: b :: rs, acc)
      case OrRule(a, _) :: rs                           => go(a :: rs, acc)
      case IgnoreRule(r) :: rs                          => go(r :: rs, acc)
      case MapRule(r, _) :: rs                          => go(r :: rs, acc)
      case (EmptyRule() | CaptureRule(_)) :: rs         => go(rs, acc)
    }
    go(List(rule), Nil).reverse
  }

  private val uriTemplate =
    for (q <- UriConverter.createQuery(rule))
      yield UriTemplate(query = q)

  override def asUriTemplate(request: Request[F]) =
    UriConvertible.respectPathInfo(uriTemplate, request)
}

object TypedQuery {
  import shapeless.{HNil, :: => :#:}

  implicit def queryParamKeyLike[F[_], T]: QueryParamKeyLike[TypedQuery[F, T:#:HNil]] = new QueryParamKeyLike[TypedQuery[F, T:#:HNil]] {
    override def getKey(t: TypedQuery[F, T:#:HNil]): QueryParameterKey =
      getKey(t.rule).getOrElse(sys.error("Empty Query doesn't have a key name"))

    private def getKey(rule: RequestRule[F]): Option[QueryParameterKey] = rule match {
      case MetaRule(_,QueryMetaData(n,_,_,_,_)) => Some(QueryParameterKey(n))
      case MetaRule(r,_)                        => getKey(r)
      case OrRule(a, b)                         => getKey(a) orElse getKey(b)
      case IgnoreRule(r)                        => getKey(r)
      case AndRule(a, b)                        => getKey(a) orElse getKey(b) // shouldn't get here
      case MapRule(r, _)                        => getKey(r)                  // shouldn't get here
      case CaptureRule(_)                       => None                       // shouldn't get here
      case EmptyRule()                          => None                       // shouldn't get here
    }
  }
}
