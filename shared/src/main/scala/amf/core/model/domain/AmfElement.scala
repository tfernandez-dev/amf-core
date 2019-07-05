package amf.core.model.domain

import amf.core.annotations.{LexicalInformation, LocalElement, SourceLocation, TrackedElement}
import amf.core.iterator.{AmfIterator, CompleteIterator, DomainElementIterator}
import amf.core.parser.Annotations

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Amf element including DomainElements and BaseUnits
  */
trait AmfElement {

  /** Set of annotations for element. */
  val annotations: Annotations

  /** Add specified annotation. */
  def add(annotation: Annotation): this.type = {
    annotations += annotation
    this
  }

  /** Merge specified annotations. */
  def add(other: Annotations): this.type = {
    annotations ++= other
    this
  }

  /** search for position in annotations */
  def position(): Option[LexicalInformation] = annotations.find(classOf[LexicalInformation])

  /** search for location in annotations */
  def location(): Option[String] = annotations.find(classOf[SourceLocation]).map(_.location)

  /** true if the element have the local annotation, that means that has been aggregated in resolution. e.g: local examples to declared type */
  def fromLocal(): Boolean = annotations.find(classOf[LocalElement]).isDefined

  def isTrackedBy(trackId: String): Boolean =
    annotations.collect { case t: TrackedElement if t.parents.contains(trackId) => t }.nonEmpty

  /** Recursive traversal through model collecting [T] based on partial function. */
  def collect[T](strategy: IteratorStrategy = DomainElementStrategy)(pf: PartialFunction[AmfElement, T]): Seq[T] = {
    val builder = new ListBuffer[T]()
    strategy.iterator(this).foreach(pf.runWith(builder.append(_)))
    builder.result()
  }

  def collectFirst[T](strategy: IteratorStrategy = DomainElementStrategy)(pf: PartialFunction[AmfElement, T]): Option[T] = {
    val it = strategy.iterator(this)
    while(it.hasNext){
      val next = it.next
      if(pf.isDefinedAt(next)) return Some(pf.apply(next))
    }
    None
  }

}

trait IteratorStrategy{
  def iterator(element: AmfElement): AmfIterator
}

object CompleteStrategy extends  IteratorStrategy {
  override def iterator(element: AmfElement): AmfIterator = new CompleteIterator(element)
}

object DomainElementStrategy extends  IteratorStrategy {
  override def iterator(element: AmfElement): AmfIterator = new DomainElementIterator(element)
}
