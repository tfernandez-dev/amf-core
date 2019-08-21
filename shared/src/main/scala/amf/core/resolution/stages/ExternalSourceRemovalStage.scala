package amf.core.resolution.stages

import amf.core.metamodel.domain.ExternalSourceElementModel
import amf.core.model.document.BaseUnit
import amf.core.model.domain.{DomainElement, ExternalSourceElement}
import amf.core.parser.ErrorHandler
import amf.core.resolution.stages.selectors.{ExternalSourceElementSelector, KnownElementIdSelector}

import scala.collection.mutable

class ExternalSourceRemovalStage(val visited: mutable.Set[String] = mutable.Set())(
    override implicit val errorHandler: ErrorHandler)
    extends ResolutionStage {

  override def resolve[T <: BaseUnit](model: T): T = {
    val knownIdSelector = new KnownElementIdSelector(visited)
    model.transform(knownIdSelector || ExternalSourceElementSelector, transformation).asInstanceOf[T]
  }

  private def transformation(element: DomainElement, cycle: Boolean): Option[DomainElement] = {
    element match {
      case ex: ExternalSourceElement =>
        ex.fields.removeField(ExternalSourceElementModel.ReferenceId)
        Some(ex)
      case _ => Some(element)
    }
  }
}
