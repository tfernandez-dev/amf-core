package amf.client.reference

import amf.client.convert.CoreClientConverters.ClientFuture

import scala.scalajs.js

@js.native
trait ClientReferenceResolver extends js.Object {

  /** Fetch specified reference and return associated [[CachedReference]]. Resource should have benn previously accepted. */
  /** If the resource not exists, you should return a future failed with an ResourceNotFound exception. */
  def fetch(url: String): ClientFuture[CachedReference] = js.native

}
