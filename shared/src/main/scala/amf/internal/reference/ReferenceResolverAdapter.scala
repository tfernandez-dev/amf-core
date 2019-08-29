package amf.internal.reference

import amf.client.convert.CoreClientConverters._
import amf.client.reference.{ReferenceResolver => ClientReferenceResolver}

import scala.concurrent.Future

/** Adapts a client ReferenceResolver to an internal one. */
case class ReferenceResolverAdapter(private[amf] val adaptee: ClientReferenceResolver) extends ReferenceResolver {

  override def fetch(url: String): Future[CachedReference] = adaptee.fetch(url).asInternal
}
