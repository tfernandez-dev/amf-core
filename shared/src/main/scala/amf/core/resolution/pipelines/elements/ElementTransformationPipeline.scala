package amf.core.resolution.pipelines.elements

import amf.core.model.domain.DomainElement
import amf.core.parser.ErrorHandler
import amf.core.resolution.stages.elements.resolution.{ElementStageTransformer, ReferenceResolution}

abstract class ElementTransformationPipeline[T<:DomainElement](element:T, errorHandler:ErrorHandler) {

  val steps: Seq[ElementStageTransformer[T]]

  val references = new ReferenceResolution(keepEditingInfo = false, modelResolver = None, errorHandler = errorHandler)

  final def resolve(): T = {
    var result: T = references.transform(element).map(_.asInstanceOf[T]).getOrElse(element)
    steps.foreach { s =>
      s.transform(result).foreach( result = _)
    }
    result
  }

}