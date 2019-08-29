package amf.client.reference

import amf.client.convert.CoreClientConverters._
import amf.internal.reference.{CachedReference => InternalCachedReference}
import amf.core.model.document.{BaseUnit => InternalBaseUnit}
import amf.client.model.document.BaseUnit

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
trait ReferenceResolver {

  /** Fetch specified reference and return associated [CachedReference]. */
  /** If the resource not exists, you should return a future failed with an [[ResourceNotFound]] exception. */
  def fetch(url: String): ClientFuture[CachedReference]
}

@JSExportAll
case class CachedReference private[amf] (private[amf] val _internal: InternalCachedReference) {

  @JSExportTopLevel("client.remote.CachedReference")
  def this(url: String, content: InternalBaseUnit, resolved: Boolean) =
    this(InternalCachedReference(url, content, resolved))

  def url: String       = _internal.url
  def content: BaseUnit = _internal.content
  def resolved: Boolean = _internal.resolved
}
