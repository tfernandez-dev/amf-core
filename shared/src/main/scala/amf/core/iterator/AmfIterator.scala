package amf.core.iterator
import amf.core.model.domain.AmfElement

trait AmfIterator {
  def hasNext: Boolean
  def next: AmfElement
  def foreach[U](f: AmfElement => U): Unit =
    while(hasNext){
      f(next)
    }

}