package amf.core.rdf

import amf.core.model.document.SourceMap
import amf.core.model.domain.{AmfElement, Annotation, ResolvableAnnotation}
import amf.core.parser.Annotations

import scala.collection.mutable

trait RdfParserCommon {

  implicit val ctx: RdfParserContext

  def annots(sources: SourceMap, key: String) = annotations(ctx.nodes, sources, key).into(ctx.collected, _.isInstanceOf[ResolvableAnnotation])

  private def annotations(nodes: Map[String, AmfElement], sources: SourceMap, key: String): Annotations = {
    val result = Annotations()

    if (sources.nonEmpty) {
      sources.annotations.foreach {
        case (annotation, values: mutable.Map[String, String]) =>
          annotation match {
            case Annotation(deserialize) if values.contains(key) =>
              deserialize(values(key), nodes).foreach(result += _)
            case _ =>
          }
      }
    }

    result
  }
}
