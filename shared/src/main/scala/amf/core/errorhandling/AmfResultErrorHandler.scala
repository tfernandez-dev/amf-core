package amf.core.errorhandling
import amf.core.annotations.LexicalInformation
import amf.core.validation.AMFValidationResult

trait AmfResultErrorHandler extends ErrorHandler {

  def handlerAmfResult(result:AMFValidationResult):Boolean

  override def reportConstraint(id: String,
                                node: String,
                                property: Option[String],
                                message: String,
                                lexical: Option[LexicalInformation],
                                level: String,
                                location: Option[String]): Unit = handlerAmfResult(AMFValidationResult(message, level, node, property, id, lexical, location, this))

    // todo: add report builder.
}
