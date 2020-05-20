package amf.core.rdf.converter

import amf.core.errorhandling.ErrorHandler
import amf.core.metamodel.Type
import amf.core.metamodel.Type._
import amf.core.model.domain.AmfScalar
import amf.core.rdf.{Literal, PropertyObject, Uri}
import amf.plugins.features.validation.CoreValidations.UnableToConvertToScalar
import org.mulesoft.common.time.SimpleDateTime

// Left as object to avoid creating instances of it due to AMF Service request.
object StringIriUriRegexParser {
  def parse(property: PropertyObject): AmfScalar = AmfScalar(s"${property.value}")
}

/**
  * TODO this has very similar logic to AnyTypeConverter, we need to review why are we first match by type in tryConvert
  * and then we match by PropertyObject in the same way as we do in AnyTypeConverter. Maybe these logics can be merged or
  * make one dependent on the other. Furthermore check why are we including the extra cases for Iri, Str, RegExp and
  * LiteralUri here and not in AnyTypeConverter.
  */
// Left as object to avoid creating instances of it due to AMF Service request.
object ScalarTypeConverter extends Converter {
  def tryConvert(`type`: Type, property: PropertyObject)(implicit errorHandler: ErrorHandler): Option[AmfScalar] = {
    `type` match {
      case Iri | Str | RegExp | LiteralUri => Some(StringIriUriRegexParser.parse(property))
      case Bool                            => bool(property)
      case Type.Int                        => int(property)
      case Type.Float                      => float(property)
      case Type.Double                     => double(property)
      case Type.DateTime | Type.Date       => date(property)
      case _                               => None
    }
  }

  def bool(property: PropertyObject)(implicit errorHandler: ErrorHandler): Option[AmfScalar] = {
    property match {
      case Literal(v, _) => Some(AmfScalar(v.toBoolean))
      case Uri(v)        => conversionValidation(s"Expecting Boolean literal found URI $v")
    }
  }

  def int(property: PropertyObject)(implicit errorHandler: ErrorHandler): Option[AmfScalar] = {
    property match {
      case Literal(v, _) => Some(AmfScalar(v.toInt))
      case Uri(v)        => conversionValidation(s"Expecting Int literal found URI $v")
    }
  }

  def double(property: PropertyObject)(implicit errorHandler: ErrorHandler): Option[AmfScalar] = {
    property match {
      case Literal(v, _) => Some(AmfScalar(v.toDouble))
      case Uri(v)        => conversionValidation(s"Expecting Double literal found URI $v")
    }
  }

  def date(property: PropertyObject)(implicit errorHandler: ErrorHandler): Option[AmfScalar] = {
    property match {
      case Literal(v, _) =>
        SimpleDateTime.parse(v) match {
          case Right(value) => Some(AmfScalar(value))
          case Left(error)  => conversionValidation(error.message)
        }
      case Uri(v) => conversionValidation(s"Expecting Date literal found URI $v")
    }
  }

  def float(property: PropertyObject)(implicit errorHandler: ErrorHandler): Option[AmfScalar] = {
    property match {
      case Literal(v, _) => Some(AmfScalar(v.toFloat))
      case Uri(v)        => conversionValidation(s"Expecting Float literal found URI $v")
    }
  }
}
