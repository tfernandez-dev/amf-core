package amf.core.rdf.helper

import amf.core.rdf.Node
import amf.core.vocabulary.Namespace

class DefaultNodeClassSorter() {

  private val deferredTypesSet = Set(
    (Namespace.Document + "Document").iri(),
    (Namespace.Document + "Fragment").iri(),
    (Namespace.Document + "Module").iri(),
    (Namespace.Document + "Unit").iri(),
    (Namespace.Shacl + "Shape").iri(),
    (Namespace.Shapes + "Shape").iri()
  )

  def sortedClassesOf(node: Node): Seq[String] = {
    node.classes.partition(deferredTypesSet.contains) match {
      case (deferred, others) => others ++ deferred.sorted // we just use the fact that lexical order is correct
    }
  }
}
