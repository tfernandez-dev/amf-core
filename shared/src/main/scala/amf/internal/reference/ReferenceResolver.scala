package amf.internal.reference

import amf.core.model.document.BaseUnit

import scala.concurrent.Future

trait ReferenceResolver {
  /** Fetch specified reference and return associated cached reference if exists. */
  def fetch(url: String): Future[CachedReference]
}

case class CachedReference(url: String, content: BaseUnit, resolved: Boolean)