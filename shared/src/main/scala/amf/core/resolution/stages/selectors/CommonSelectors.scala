package amf.core.resolution.stages.selectors

import amf.core.metamodel.domain.ExternalSourceElementModel
import amf.core.metamodel.{MetaModelTypeMapping, Obj}
import amf.core.model.domain.{DomainElement, ExternalSourceElement, LinkNode, Linkable}
import amf.core.vocabulary.{Namespace, ValueType}

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

object ShapeSelector extends Selector with MetaModelTypeMapping {
  override def apply(element: DomainElement): Boolean = {
    // Why don't we check directly if the domain element is an instance of Shape?
    val metaModelFound: Obj = metaModel(element)
    val targetIri           = (Namespace.Shapes + "Shape").iri()
    metaModelFound.`type`.exists { t: ValueType =>
      t.iri() == targetIri
    }
  }
}

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
