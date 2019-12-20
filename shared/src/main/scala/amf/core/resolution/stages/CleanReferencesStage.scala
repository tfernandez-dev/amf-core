package amf.core.resolution.stages

import amf.core.errorhandling.ErrorHandler
import amf.core.metamodel.document.BaseUnitModel
import amf.core.model.document.BaseUnit

class CleanReferencesStage()(override implicit val errorHandler: ErrorHandler) extends ResolutionStage() {
  override def resolve[T <: BaseUnit](model: T): T = {
    model.fields.removeField(BaseUnitModel.References)
    model.asInstanceOf[T]
  }
}
