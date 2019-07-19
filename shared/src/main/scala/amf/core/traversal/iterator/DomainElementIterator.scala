package amf.core.traversal.iterator
import amf.core.model.domain.{AmfArray, AmfElement, AmfObject, DomainElement}

import scala.collection.mutable

class DomainElementIterator(var buffer: List[AmfElement], visited: mutable.Set[String]) extends AmfIterator {

  def this(elements: List[AmfElement]) = {
    this(elements, mutable.Set())
    advance()
  }

  override def hasNext: Boolean = buffer.nonEmpty

  override def next: AmfElement = {
    val current = buffer.head
    buffer = buffer.tail
    advance()
    current
  }

  private def advance(): Unit = {
    if(buffer.nonEmpty) {
      val current = buffer.head
      buffer = buffer.tail
      current match {
        case obj: AmfObject =>
          if (visited.contains(obj.id)) {
            advance()
          } else {
            val elements = obj.fields.fields().map(_.element).toList
            visited += obj.id
            obj match {
              case domain: DomainElement =>
                buffer = domain :: elements ++ buffer
              // advance finishes here because a non visited domain element was found
              case _ =>
                buffer = elements ++ buffer
                advance()
            }
          }
        case arr: AmfArray =>
          buffer = arr.values.toList ++ buffer
          advance()
        case _ =>
          advance()
      }
    }
  }

}
