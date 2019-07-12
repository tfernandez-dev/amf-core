package amf.core.iterator

import amf.core.model.domain.AmfElement

trait IteratorStrategy {
  def iterator(elements: List[AmfElement]): AmfIterator
}

object AmfElementStrategy extends IteratorStrategy {
  override def iterator(elements: List[AmfElement]): AmfIterator =
    new AmfElementIterator(elements)
}

object DomainElementStrategy extends IteratorStrategy {
  override def iterator(elements: List[AmfElement]): AmfIterator =
    new DomainElementIterator(elements)
}
