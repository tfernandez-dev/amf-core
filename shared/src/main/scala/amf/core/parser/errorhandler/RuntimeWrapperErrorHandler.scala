package amf.core.parser.errorhandler
import amf.core.annotations.LexicalInformation

abstract class RuntimeWrapperErrorHandler(parent:ParserErrorHandler) extends ParserErrorHandler{


  override def reportConstraint(id: String,
                                node: String,
                                property: Option[String],
                                message: String,
                                lexical: Option[LexicalInformation],
                                level: String,
                                location: Option[String]): Unit = parent.reportConstraint(id, node, property, message, lexical,level,location)

}
