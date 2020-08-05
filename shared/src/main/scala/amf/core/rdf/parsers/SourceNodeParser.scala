package amf.core.rdf.parsers

import amf.core.metamodel.document.SourceMapModel
import amf.core.model.document.SourceMap
import amf.core.rdf.graph.NodeFinder
import amf.core.rdf.{Node, RDFTerm, RdfModel, Uri}
import amf.core.vocabulary.Namespace.SourceMaps

class SourceNodeParser(linkFinder: NodeFinder) {

  def parse(node: Node): SourceMap = {
    val result = SourceMap()
    node.getKeys().foreach {
      case key @ AnnotationName(annotation) =>
        val consumer = result.annotation(annotation)
        node.getProperties(key) match {
          case Some(properties) =>
            properties.foreach { property =>
              linkFinder.findLink(property) match {
                case Some(linkedNode) =>
                  val k: RDFTerm = linkedNode.getProperties(SourceMapModel.Element.value.iri()).get.head
                  val v: RDFTerm = linkedNode.getProperties(SourceMapModel.Value.value.iri()).get.head
                  consumer(k.value, v.value)
                case _ => //
              }
            }

          case _ => // ignore
        }
      case _ => // Unknown annotation identifier
    }
    result
  }
}

object AnnotationName {
  def unapply(uri: String): Option[String] = uri match {
    case url if url.startsWith(SourceMaps.base) => Some(url.substring(url.indexOf("#") + 1))
    case _                                      => None
  }
}
