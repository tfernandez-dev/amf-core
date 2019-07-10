package amf.core.iterator
import amf.core.model.domain.{AmfArray, AmfElement, AmfObject, DomainElement}

import scala.collection.mutable

class DomainElementIterator(var buffer: List[AmfElement], visited: mutable.Set[String]) extends AmfElementIterator {

  def this(element: AmfElement) = {
    this(List(element), mutable.Set())
    advanceToNextDomainElement()
  }

  override def hasNext: Boolean = buffer.nonEmpty

  override def next: AmfElement = {
    val current = buffer.head
    buffer = buffer.tail
    advanceToNextDomainElement()
    current
  }

  private def advanceToNextDomainElement(): Unit = {
    if(buffer.nonEmpty) {
      val current = buffer.head
      buffer = buffer.tail
      current match {
        case domain: DomainElement =>
          if (visited.contains(domain.id)) {
            advanceToNextDomainElement()
          } else {
            val elements = domain.fields.fields().map(_.value.value).toList
            visited += domain.id
            buffer = current :: elements ++ buffer
            // unico caso donde freno porque encontre un domain element que no fue visitado
          }
        case obj: AmfObject =>
          if (visited.contains(obj.id)) {
            advanceToNextDomainElement()
          } else {
            val elements = obj.fields.fields().map(_.value.value).toList
            visited += obj.id
            buffer = elements ++ buffer
            advanceToNextDomainElement()
          }
        case arr: AmfArray =>
          buffer = arr.values.toList ++ buffer
          advanceToNextDomainElement()
        case _ =>
          advanceToNextDomainElement()
      }
    }
  }

}
