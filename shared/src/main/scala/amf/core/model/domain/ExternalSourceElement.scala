package amf.core.model.domain

import amf.core.metamodel.Field
import amf.core.metamodel.domain.ExternalSourceElementModel._
import amf.core.model.StrField

trait ExternalSourceElement extends DomainElement {

  def raw: StrField         = fields.field(Raw)         //we should set this while parsing
  def referenceId: StrField = fields.field(ReferenceId) /// only for graph parser logic

  override def location(): Option[String] = {

    val location: StrField = fields.field(Location)
    if (location.option().isDefined) location.option()
    else super.location()
  }

  // this its dynamic, because when graph emitter is going to serialize the raw field, first we need to check if its a link to an external fragment.
  // In that case the raw should't be emitted, and it should be only a ref to the external domain element with the raw. This its to avoid duplicated json and xml schemas definitions
  // todo: antonio add comment.

  def isLinkToSource = fields.entry(ReferenceId).isDefined

  def withReference(id: String): this.type = set(ReferenceId, id)
}
