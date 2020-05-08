package amf.core.rdf.converter

import amf.core.errorhandling.ErrorHandler
import amf.core.metamodel.Type
import amf.core.metamodel.Type._
import amf.core.model.domain.AmfScalar
import amf.core.rdf.{Literal, PropertyObject, Uri}
import amf.plugins.features.validation.CoreValidations.UnableToConvertToScalar
import org.mulesoft.common.time.SimpleDateTime

case class ScalarTypeConverter(`type`: Type, property: PropertyObject)(implicit errorHandler: ErrorHandler) {
  def tryConvert(): Option[AmfScalar] = `type` match {
    case Iri | Str | RegExp | LiteralUri => Some(strCoercion(property))
    case Bool                            => bool(property)
    case Type.Int                        => int(property)
    case Type.Float                      => float(property)
    case Type.Double                     => double(property)
    case Type.DateTime | Type.Date       => date(property)
    case _                               => None
  }

  def strCoercion(property: PropertyObject): AmfScalar = AmfScalar(s"${property.value}")

  def bool(property: PropertyObject): Option[AmfScalar] = {
    property match {
      case Literal(v, _) => Some(AmfScalar(v.toBoolean))
      case Uri(v)        => conversionValidation(s"Expecting Boolean literal found URI $v")
    }
  }

  private def conversionValidation(message: String) = {
    errorHandler.violation(UnableToConvertToScalar, "", message, "")
    None
  }

  def int(property: PropertyObject): Option[AmfScalar] = {
    property match {
      case Literal(v, _) => Some(AmfScalar(v.toInt))
      case Uri(v)        => conversionValidation(s"Expecting Int literal found URI $v")
    }
  }

  def double(property: PropertyObject): Option[AmfScalar] = {
    property match {
      case Literal(v, _) => Some(AmfScalar(v.toDouble))
      case Uri(v)        => conversionValidation(s"Expecting Double literal found URI $v")
    }
  }

  def date(property: PropertyObject): Option[AmfScalar] = {
    property match {
      case Literal(v, _) =>
        SimpleDateTime.parse(v) match {
          case Right(value) => Some(AmfScalar(value))
          case Left(error)  => conversionValidation(error.message)
        }
      case Uri(v) => conversionValidation(s"Expecting Date literal found URI $v")
    }
  }

  def float(property: PropertyObject): Option[AmfScalar] = {
    property match {
      case Literal(v, _) => Some(AmfScalar(v.toFloat))
      case Uri(v)        => conversionValidation(s"Expecting Float literal found URI $v")
    }
  }
}
