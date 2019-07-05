package amf.core.iterator
import amf.core.model.domain.{AmfArray, AmfElement, AmfObject}

import scala.collection.mutable



class CompleteIterator(buffer: mutable.Queue[AmfElement], visited: mutable.Set[String]) extends AmfIterator {

  def this(element: AmfElement) = {
    this(mutable.Queue(element), mutable.Set())
  }

  override def hasNext: Boolean = {
    if(buffer.isEmpty) false
    else buffer.head match {
      case obj: AmfObject =>
        if (visited.contains(obj.id)) {
          buffer.dequeue()
          hasNext
        } else true
      case _ => true
    }
  }

  override def next: AmfElement = {
    val current = buffer.dequeue()
    current match {
      case obj: AmfObject =>
        val elements = obj.fields.fields().map(_.value.value).toSeq //TODO sacar toSeq
        elements.foreach(buffer.enqueue(_))
        visited += obj.id
      case arr: AmfArray =>
        arr.values.foreach(buffer.enqueue(_))
      case _ =>
    }
    current
  }

}





