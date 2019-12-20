package amf.core.resolution.stages

import amf.core.errorhandling.ErrorHandler
import amf.core.model.document.BaseUnit

abstract class ResolutionStage()(implicit val errorHandler: ErrorHandler) {
  def resolve[T <: BaseUnit](model: T): T
}
