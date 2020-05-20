package amf.core.rdf.converter

import amf.core.errorhandling.ErrorHandler
import amf.plugins.features.validation.CoreValidations.UnableToConvertToScalar

trait Converter {

  protected def conversionValidation(message: String)(implicit errorHandler: ErrorHandler) = {
    errorHandler.violation(UnableToConvertToScalar, "", message, "")
    None
  }
}
