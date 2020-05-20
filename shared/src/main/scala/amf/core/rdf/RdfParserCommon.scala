package amf.core.rdf

import amf.core.metamodel.Type
import amf.core.metamodel.Type.{Array, Bool, Iri, RegExp, Str}
import amf.core.model.document.SourceMap
import amf.core.model.domain.{AmfElement, Annotation, ResolvableAnnotation}
import amf.core.parser.Annotations
import org.yaml.model.{YMap, YNode, YType}

import scala.collection.mutable

import amf.core.parser._


trait RdfParserCommon {

  implicit val ctx: RdfParserContext

  def annots(sources: SourceMap, key: String): Annotations = annotations(ctx.nodes, sources, key).into(ctx.collected, _.isInstanceOf[ResolvableAnnotation])

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

  @scala.annotation.tailrec
  final def value(t: Type, node: YNode): YNode = {
    node.tagType match {
      case YType.Seq =>
        t match {
          case Array(_) => node
          case _        => value(t, node.as[Seq[YNode]].head)
        }
      case YType.Map =>
        val m: YMap = node.as[YMap]
        t match {
          case Iri                                       => m.key("@id").get.value
          case Str | RegExp | Bool | Type.Int | Type.Any => m.key("@value").get.value
          case _                                         => node
        }
      case _ => node
    }
  }
}
