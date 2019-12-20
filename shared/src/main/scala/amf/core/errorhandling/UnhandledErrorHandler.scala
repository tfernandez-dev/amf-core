package amf.core.errorhandling
import amf.core.annotations.LexicalInformation

trait UnhandledErrorHandler extends ErrorHandler{

  override def reportConstraint(id: String,
                                node: String,
                                property: Option[String],
                                message: String,
                                lexical: Option[LexicalInformation],
                                level: String,
                                location: Option[String]): Unit = {
    throw new Exception(
      s"  Message: $message\n  Target: $node\nProperty: ${property.getOrElse("")}\n  Position: $lexical\n at location: $location")
  }
}
object UnhandledErrorHandler extends UnhandledErrorHandler{}
