package amf.core.resolution.stages

import amf.core.annotations.Declares
import amf.core.errorhandling.ErrorHandler
import amf.core.metamodel.document.DocumentModel
import amf.core.model.document.{BaseUnit, DeclaresModel, EncodesModel}
import amf.core.model.domain.AmfArray

class DeclarationsRemovalStage()(override implicit val errorHandler: ErrorHandler) extends ResolutionStage() {

  override def resolve[T <: BaseUnit](model: T): T = {
    model match {
      case doc: DeclaresModel with EncodesModel => removeAllDeclarationsButSecuritySchemes(doc)
      case _                                    => // ignore
    }
    model.asInstanceOf[T]
  }

  private def removeAllDeclarationsButSecuritySchemes(doc: DeclaresModel) = {
    val schemes = doc.declares.filter(_.meta.`type`.head.iri() == "http://a.ml/vocabularies/security#SecurityScheme")
    persistDeclaredShapes(doc)
    if (schemes.isEmpty) {
      doc.fields.removeField(DocumentModel.Declares)
    } else {
      doc.fields.?[AmfArray](DocumentModel.Declares) match {
        case Some(array) => array.values = schemes
        case _           =>
      }
    }
  }

  private def persistDeclaredShapes(doc: DeclaresModel): Unit = {
    val declaredShapes = doc.declares.map(_.id)
    doc.annotations += Declares(declaredShapes)
  }
}
