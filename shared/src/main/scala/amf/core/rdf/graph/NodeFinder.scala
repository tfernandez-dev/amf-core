package amf.core.rdf.graph

import amf.core.rdf.{RDFTerm, RdfModel, Uri}

class NodeFinder(graph: RdfModel) {
  def findLink(property: RDFTerm) = {
    property match {
      case Uri(v) => graph.findNode(v)
      case _      => None
    }
  }
}
