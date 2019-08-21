package amf.core.resolution.stages.selectors

import amf.core.model.domain.DomainElement

abstract case class Selector() extends Function[DomainElement, Boolean] {
  def ||(another: Selector): Selector = {
    new OrSelector(this, another)
  }

  def &&(another: Selector): Selector = {
    new AndSelector(this, another)
  }

  def not(): Selector = {
    new NotSelector(this)
  }
}

private class OrSelector(s0: Selector, s1: Selector) extends Selector {
  override def apply(element: DomainElement): Boolean = {
    s0.apply(element) || s1.apply(element)
  }
}

private class AndSelector(s0: Selector, s1: Selector) extends Selector {
  override def apply(element: DomainElement): Boolean = {
    s0.apply(element) && s1.apply(element)
  }
}

private class NotSelector(s0: Selector) extends Selector {
  override def apply(element: DomainElement): Boolean = {
    !s0.apply(element)
  }
}
