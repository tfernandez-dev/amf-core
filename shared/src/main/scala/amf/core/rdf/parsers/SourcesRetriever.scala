package amf.core.rdf.parsers

import amf.core.metamodel.domain.DomainElementModel
import amf.core.model.document.SourceMap
import amf.core.rdf.Node
import amf.core.rdf.graph.NodeFinder

class SourcesRetriever(linkFinder: NodeFinder) {
  def retrieve(node: Node): SourceMap = {
    node
      .getProperties(DomainElementModel.Sources.value.iri())
      .flatMap { properties =>
        if (properties.nonEmpty) {
          linkFinder.findLink(properties.head) match {
            case Some(sourceNode) => Some(new SourceNodeParser(linkFinder).parse(sourceNode))
            case _                => None
          }
        } else {
          None
        }
      }
      .getOrElse(SourceMap.empty)
  }
}
