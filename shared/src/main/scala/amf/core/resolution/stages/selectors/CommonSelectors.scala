package amf.core.resolution.stages.selectors

import amf.core.metamodel.domain.ExternalSourceElementModel
import amf.core.metamodel.{Obj, MetaModelTypeMapping}
import amf.core.model.domain.{DomainElement, LinkNode, Linkable, ExternalSourceElement}
import amf.core.vocabulary.Namespace._
import amf.core.vocabulary.{ValueType, Namespace}

import scala.collection.mutable

object LinkSelector extends Selector {
  override def apply(element: DomainElement): Boolean =
    element match {
      case l: Linkable => l.isLink
      case _           => false
    }
}

object LinkNodeSelector extends Selector {
  override def apply(element: DomainElement): Boolean = {
    element match {
      case _: LinkNode => true
      case _           => false
    }
  }
}

abstract class MetaModelSelector(namespace: Namespace, id: String) extends Selector with MetaModelTypeMapping {
  override def apply(element: DomainElement): Boolean = {
    val metaModelFound: Obj = metaModel(element)
    val targetIri           = (namespace + id).iri()
    metaModelFound.`type`.exists { t: ValueType =>
      t.iri() == targetIri
    }
  }
}

object ShapeSelector extends MetaModelSelector(Shapes, "Shape")

object PropertyShapeSelector extends MetaModelSelector(Shacl, "property")

object NodeShapeSelector extends MetaModelSelector(Shacl, "NodeShape")

object ExternalSourceElementSelector extends Selector {
  override def apply(element: DomainElement): Boolean = {
    element match {
      case ex: ExternalSourceElement => ex.fields.exists(ExternalSourceElementModel.ReferenceId)
      case _                         => false
    }
  }
}

class KnownElementIdSelector(visited: mutable.Set[String]) extends Selector {
  override def apply(element: DomainElement): Boolean = {
    if (visited.contains(element.id)) {
      true
    } else {
      visited += element.id
      false
    }
  }
}
