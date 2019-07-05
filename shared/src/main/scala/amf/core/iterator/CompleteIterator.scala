package amf.core.iterator
import amf.core.model.domain.{AmfArray, AmfElement, AmfObject}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer



class CompleteIterator(var buffer: List[AmfElement], visited: mutable.Set[String]) extends AmfElementIterator {

  def this(element: AmfElement) = {
    this(List(element), mutable.Set())
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
            val elements = obj.fields.fields().map(_.value.value)
            buffer = current :: elements.toList ++ buffer
            visited += obj.id
          }
        case arr: AmfArray =>
          buffer = current :: arr.values.toList ++ buffer
        case _ =>
          buffer = current :: buffer
      }
    }
  }

}





