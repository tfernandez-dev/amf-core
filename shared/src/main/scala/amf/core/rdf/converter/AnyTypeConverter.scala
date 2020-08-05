package amf.core.rdf.converter

import amf.core.errorhandling.ErrorHandler
import amf.core.model.DataType
import amf.core.model.domain.AmfScalar
import amf.core.rdf.{Literal, RDFTerm, Uri}
import amf.plugins.features.validation.CoreValidations.UnableToConvertToScalar
import org.mulesoft.common.time.SimpleDateTime

// Left as object to avoid creating instances of it due to AMF Service request.
object AnyTypeConverter extends Converter {
  private val xsdString: String   = DataType.String
  private val xsdInteger: String  = DataType.Integer
  private val xsdFloat: String    = DataType.Float
  private val amlNumber: String   = DataType.Number
  private val xsdDouble: String   = DataType.Double
  private val xsdBoolean: String  = DataType.Boolean
  private val xsdDateTime: String = DataType.DateTime
  private val xsdDate: String     = DataType.Date

  def tryConvert(property: RDFTerm)(implicit errorHandler: ErrorHandler): Option[AmfScalar] = {
    property match {
      case Literal(v, typed) =>
        typed match {
          case Some(s: String) if s == xsdBoolean  => Some(AmfScalar(v.toBoolean))
          case Some(s: String) if s == xsdInteger  => Some(AmfScalar(v.toInt))
          case Some(s: String) if s == xsdFloat    => Some(AmfScalar(v.toFloat))
          case Some(s: String) if s == xsdDouble   => Some(AmfScalar(v.toDouble))
          case Some(s: String) if s == xsdDateTime => Some(AmfScalar(SimpleDateTime.parse(v).right.get))
          case Some(s: String) if s == xsdDate     => Some(AmfScalar(SimpleDateTime.parse(v).right.get))
          case _                                   => Some(AmfScalar(v))
        }
      case Uri(v) => conversionValidation(s"Expecting String literal found URI $v")
    }
  }
}
