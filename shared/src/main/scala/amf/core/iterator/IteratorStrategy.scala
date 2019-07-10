package amf.core.iterator

import amf.core.model.domain.AmfElement

trait IteratorStrategy {
  def iterator(element: AmfElement): AmfElementIterator
}

object CompleteStrategy extends IteratorStrategy {
  override def iterator(element: AmfElement): AmfElementIterator =
    new CompleteIterator(element)
}

object DomainElementStrategy extends IteratorStrategy {
  override def iterator(element: AmfElement): AmfElementIterator =
    new DomainElementIterator(element)
}
