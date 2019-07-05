package amf.core.iterator
import amf.core.model.domain.{AmfArray, AmfElement, AmfObject, DomainElement}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DomainElementIterator(buffer: mutable.Queue[AmfElement], visited: mutable.Set[String]) extends AmfIterator {

  def this(element: AmfElement) = {
    this(mutable.Queue(element), mutable.Set())
  }

  override def hasNext: Boolean = {
    if(buffer.exists {
      case element: DomainElement if !visited.contains(element.id) => true
      case _ => false
    }) true
    else traverseToFindFirstDomainElement()

  }

  private def traverseToFindFirstDomainElement(): Boolean = {
    if(buffer.isEmpty) false
    else {
      buffer.head match {
        case domain: DomainElement  =>
          if (!visited.contains(domain.id)) true
          else {
            buffer.dequeue()
            traverseToFindFirstDomainElement()
          }
        case obj: AmfObject =>
          buffer.dequeue()
          if(!visited.contains(obj.id)){
            visited += obj.id
            val elements = obj.fields.fields().map(_.value.value)
            elements.foreach(buffer.enqueue(_))
          }
          traverseToFindFirstDomainElement()
        case arr: AmfArray =>
          buffer.dequeue()
          arr.values.foreach(buffer.enqueue(_))
          traverseToFindFirstDomainElement()
        case _ =>
          buffer.dequeue()
          traverseToFindFirstDomainElement()

      }
    }
  }


  override def next: AmfElement = {
    val current = buffer.dequeue()
    current match {
      case domain: DomainElement =>
        if(visited.contains(domain.id)){
          next
        }
        else{
          val elements = domain.fields.fields().map(_.value.value)
          elements.foreach(buffer.enqueue(_))
          visited += domain.id
          domain
        }
      case obj: AmfObject =>
        if(visited.contains(obj.id)){
          next
        }
        else{
          val elements = obj.fields.fields().map(_.value.value)
          elements.foreach(buffer.enqueue(_))
          visited += obj.id
          next
        }
      case arr: AmfArray =>
        arr.values.foreach(buffer.enqueue(_))
        next

      case _ =>
        next
    }
  }
}
