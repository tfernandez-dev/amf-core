package amf.core.traversal

import amf.core.model.domain.AmfObject

import scala.collection.mutable

trait TraversalPath {
  def hasVisited(element: AmfObject): Boolean
  def traversed(element: AmfObject)
}

case class ObjectIdTraversalPath(traversed: mutable.Set[String] = mutable.Set()) extends TraversalPath {
  def hasVisited(element: AmfObject): Boolean = traversed.contains(element.id)
  def traversed(element: AmfObject): Unit     = traversed.add(element.id)
}