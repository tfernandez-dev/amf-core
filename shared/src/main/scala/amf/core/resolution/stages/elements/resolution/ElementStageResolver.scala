package amf.core.resolution.stages.elements.resolution

import amf.core.model.domain.DomainElement

abstract class ElementStageTransformer[T<:DomainElement]{

  def transform(element:T):Option[T]
}

trait ElementResolutionStage[T<:DomainElement]{

  def transformer: ElementStageTransformer[T]
}