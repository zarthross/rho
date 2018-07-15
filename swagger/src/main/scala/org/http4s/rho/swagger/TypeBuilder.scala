package org.http4s.rho.swagger

import java.util.Date
import java.time.Instant

import org.log4s.getLogger

import scala.reflect.runtime.universe._
import scala.util.control.NonFatal
import cats.syntax.all._
import org.http4s.rho.bits.{ResultMetadata, ResultPrimitiveMetadata}

case class DiscriminatorField(field: String) extends scala.annotation.StaticAnnotation

object TypeBuilder {
  import models._

  private[this] val logger = getLogger

  def collectModels(t: ResultMetadata.Tpe, alreadyKnown: Set[Model]): Set[Model] =
    try collectModels(t, alreadyKnown, Set.empty)
    catch { case NonFatal(_) => Set.empty }

  private def collectModels(t: ResultMetadata.Tpe, alreadyKnown: Set[Model], known: Set[Type]): Set[Model] = {

   /* def go(t: ResultMetadata.Tpe, alreadyKnown: Set[Model], known: Set[Type]): Set[Model] =
      t match {

        case tpe if tpe.isNothingOrNull || tpe.isUnitOrVoid =>
          alreadyKnown ++ modelToSwagger(tpe)

        case tpe if tpe.isEither || tpe.isMap =>
          go(tpe.typeArgs.head, alreadyKnown, tpe.typeArgs.toSet) ++
            go(tpe.typeArgs.last, alreadyKnown, tpe.typeArgs.toSet)

        case tpe if (tpe.isCollection || tpe.isOption) && tpe.typeArgs.nonEmpty =>
          val ntpe = tpe.typeArgs.head
          if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
          else Set.empty

        case tpe if tpe.isStream =>
          val ntpe = tpe.typeArgs.apply(1)
          if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
          else Set.empty

        case tpe if tpe.isEffect(et) =>
          val ntpe = tpe.typeArgs.head
          if (!known.exists(_ =:= ntpe)) go(ntpe, alreadyKnown, known + ntpe)
          else Set.empty

        case tpe if tpe.isSwaggerFile =>
          Set.empty

        case tpe if alreadyKnown.map(_.id).contains(tpe.fullName) || tpe.isPrimitive =>
          Set.empty

        case tpe if tpe <:< typeOf[AnyVal] =>
          Set.empty

        case ExistentialType(_, _) =>
          Set.empty

        case tpe@TypeRef(_, sym: Symbol, tpeArgs: List[Type]) if isCaseClass(sym) =>
          val ctor = sym.asClass.primaryConstructor.asMethod
          val models = alreadyKnown ++ modelToSwagger(tpe)
          val generics = tpe.typeArgs.foldLeft(List[Model]()) { (acc, t) =>
            acc ++ go(t, alreadyKnown, tpe.typeArgs.toSet)
          }
          val children = ctor.paramLists.flatten.flatMap { paramsym =>
            val paramType =
              if (sym.isClass)
                paramsym.typeSignature.substituteTypes(sym.asClass.typeParams, tpeArgs)
              else
                sym.typeSignature
            go(paramType, alreadyKnown, known + tpe)
          }

          models ++ generics ++ children

        case TypeRef(_, sym, _) if isObjectEnum(sym) =>
          Set.empty

        case tpe@TypeRef(_, sym, _) if isSumType(sym) =>
          // TODO promote methods on sealed trait from children to model
          modelToSwagger(tpe).map(addDiscriminator(sym)).toSet.flatMap { (model: Model) =>
            val refmodel = RefModel(model.id, model.id2, model.id2)
            val children =
              sym.asClass.knownDirectSubclasses.flatMap { sub =>
                go(sub.asType.toType, alreadyKnown, known + tpe).map(composedModel(refmodel))
              }
            alreadyKnown ++ Set(model) ++ children
          }

        case _ => Set.empty
      }

    go(t, alreadyKnown, known)*/
    //TODO:
    Set.empty
  }

  private def addDiscriminator(sym: Symbol)(model: ModelImpl): ModelImpl = {
    val typeVar = sym.annotations
      .withFilter(_.tree.tpe <:< typeOf[DiscriminatorField])
      .flatMap(_.tree.children.tail.collect { case Literal(Constant(field: String)) => field } )
      .headOption.getOrElse("type")
    val subclasses = sym.asClass.knownDirectSubclasses.map(_.asType.toType.simpleName)

    model
      .copy(
        discriminator = Some(typeVar),
        `type` = Some("object"),
        properties =
        model.properties + (typeVar -> StringProperty(required = true, enums = subclasses))
      )
  }

  private def composedModel(parent: RefModel)(subtype: Model): Model = {
    ComposedModel(
      id = subtype.id,
      id2 = subtype.id2,
      description = subtype.description,
      allOf = List(parent, subtype),
      parent = parent.some
    )
  }

  private[this] def isCaseClass(sym: Symbol): Boolean =
    sym.isClass && sym.asClass.isCaseClass && sym.asClass.primaryConstructor.isMethod

  private[this] def isSumType(sym: Symbol): Boolean =
    sym.isClass && sym.asClass.isSealed && sym.asClass.knownDirectSubclasses.forall { symbol =>
      !symbol.isModuleClass && symbol.asClass.isCaseClass
    }

  private[this] def isObjectEnum(sym: Symbol): Boolean =
    sym.asClass.isSealed && sym.asClass.knownDirectSubclasses.forall { symbol =>
      symbol.isModuleClass && symbol.asClass.isCaseClass
    }

  private def modelToSwagger(tpe: ResultMetadata.Tpe): Option[ModelImpl] =
    ModelImpl(
      id          = tpe.fullName,
      id2         = tpe.simpleName,
      description = tpe.simpleName.some,
      `type`      = "object".some,
      properties  = Map(tpe.simpleName -> typeToProperty(tpe))
    ).some

  // Turn a `Type` into the appropriate `Property` representation
  private def typeToProperty(tpe: ResultMetadata.Tpe): Property = {
    /*val TypeRef(_, ptSym: Symbol, _) = tpe

    if (tpe.isMap && !tpe.isNothingOrNull) {
      val pType = tpe.dealias.typeArgs.last
      val itemProperty = typeToProperty(pType).withRequired(false)
      MapProperty(additionalProperties = itemProperty)
    }
    else if (tpe.isCollection && !tpe.isNothingOrNull) {
      val pType = tpe.dealias.typeArgs.head
      val itemProperty = typeToProperty(pType).withRequired(false)
      ArrayProperty(items = itemProperty)
    }
    else if (tpe.isOption && !tpe.isNothingOrNull)
      typeToProperty(tpe.typeArgs.head).withRequired(false)
    else if ((isCaseClass(ptSym) || isSumType(ptSym)) && !(tpe <:< typeOf[AnyVal]))
      RefProperty(tpe.simpleName)
    else
      DataType.fromType(tpe) match {
        case DataType.ValueDataType(name, format, qName) =>
          AbstractProperty(`type` = name, description = qName, format = format)
        case DataType.ComplexDataType(name, qName) =>
          AbstractProperty(`type` = name, description = qName)
        case DataType.ContainerDataType(name, _, _) =>
          AbstractProperty(`type` = name)
        case DataType.EnumDataType(enums) =>
          StringProperty(enums = enums)
      }*/
    StringProperty(enums = Set.empty)
  }

  sealed trait DataType {
    def name: String
  }

  object DataType {

    case class ValueDataType(name: String, format: Option[String] = None, qualifiedName: Option[String] = None) extends DataType
    case class ContainerDataType(name: String, typeArg: Option[DataType] = None, uniqueItems: Boolean = false) extends DataType
    case class ComplexDataType(name: String, qualifiedName: Option[String] = None) extends DataType
    case class EnumDataType(enums: Set[String]) extends DataType { val name = "string" }

    val String = DataType("string")
    val Byte = DataType("string", Some("byte"))
    val Int = DataType("integer", Some("int32"))
    val Long = DataType("integer", Some("int64"))
    val Float = DataType("number", Some("float"))
    val Double = DataType("number", Some("double"))
    val Boolean = DataType("boolean")
    val Date = DataType("string", Some("date"))
    val DateTime = DataType("string", Some("date-time"))

    object GenList {
      def apply(): DataType = ContainerDataType("List")
      def apply(v: DataType): DataType = new ContainerDataType("List", Some(v))
    }

    object GenSet {
      def apply(): DataType = ContainerDataType("Set", uniqueItems = true)
      def apply(v: DataType): DataType = new ContainerDataType("Set", Some(v), uniqueItems = true)
    }

    object GenArray {
      def apply(): DataType = ContainerDataType("Array")
      def apply(v: DataType): DataType = new ContainerDataType("Array", Some(v))
    }

    def apply(name: String, format: Option[String] = None, qualifiedName: Option[String] = None) =
      ValueDataType(name, format, qualifiedName)

    def apply(tag: ResultPrimitiveMetadata.Tpe): DataType = fromType(tag)

    private[swagger] def fromType(t: ResultPrimitiveMetadata.Tpe): DataType = {
      /*val klass = if (t.isOptional && t.typeArgs.nonEmpty) t.typeArgs.head else t

      if (klass.isNothingOrNull || klass.isUnitOrVoid) ComplexDataType(klass.simpleName, qualifiedName = Option(klass.fullName))
      else if (isString(klass)) this.String
      else if (klass <:< typeOf[Byte] || klass <:< typeOf[java.lang.Byte]) this.Byte
      else if (klass <:< typeOf[Long] || klass <:< typeOf[java.lang.Long]) this.Long
      else if (isInt(klass)) this.Int
      else if (klass <:< typeOf[Float] || klass <:< typeOf[java.lang.Float]) this.Float
      else if (isDecimal(klass)) this.Double
      else if (isDateTime(klass)) this.DateTime
      else if (isBool(klass)) this.Boolean
      else if (klass <:< typeOf[scala.collection.Set[_]] || klass <:< typeOf[java.util.Set[_]]) {
        if (t.typeArgs.nonEmpty) GenSet(fromType(t.typeArgs.head))
        else GenSet()
      } else if (klass <:< typeOf[collection.Seq[_]] || klass <:< typeOf[java.util.List[_]]) {
        if (t.typeArgs.nonEmpty) GenList(fromType(t.typeArgs.head))
        else GenList()
      } else if (t.isArray || isCollection(klass)) {
        if (t.typeArgs.nonEmpty) GenArray(fromType(t.typeArgs.head))
        else GenArray()
      } else if (t.isStream) {
        if (t.typeArgs.nonEmpty) GenArray(fromType(t.typeArgs(1)))
        else GenArray()
      } else if (klass <:< typeOf[AnyVal]) {
        fromType(klass.members.filter(_.isConstructor).flatMap(_.asMethod.paramLists.flatten).head.typeSignature)
      } else if (isObjectEnum(klass.typeSymbol)) {
        EnumDataType(klass.typeSymbol.asClass.knownDirectSubclasses.map(_.name.toString))
      } else {
        val stt = if (t.isOption) t.typeArgs.head else t
        ComplexDataType("string", qualifiedName = Option(stt.fullName))
      }*/
      this.Boolean
    }
  }
}
