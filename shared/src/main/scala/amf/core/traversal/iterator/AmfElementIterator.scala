package amf.core.traversal.iterator
import amf.core.model.domain.{AmfArray, AmfElement, AmfObject}

import scala.collection.mutable


class AmfElementIterator(var buffer: List[AmfElement], visited: mutable.Set[String]) extends AmfIterator {

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
    if (buffer.nonEmpty) {
      val current = buffer.head
      buffer = buffer.tail
      current match {
        case obj: AmfObject =>
          if (visited.contains(obj.id)) {
            advance()
          } else {
            val elements = obj.fields.fields().map(_.element)
            visited += obj.id
            buffer = current :: elements.toList ++ buffer
          }
        case arr: AmfArray =>
          buffer = current :: arr.values.toList ++ buffer
        case _ =>
          buffer = current :: buffer
      }
    }
  }

}





