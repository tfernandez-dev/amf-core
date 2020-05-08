package amf.core.rdf.graph

import amf.core.rdf.{PropertyObject, RdfModel, Uri}

class NodeFinder(graph: RdfModel) {
  def findLink(property: PropertyObject) = {
    property match {
      case Uri(v) => graph.findNode(v)
      case _      => None
    }
  }
}
