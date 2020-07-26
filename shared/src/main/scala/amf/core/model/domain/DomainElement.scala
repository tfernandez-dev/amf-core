package amf.core.model.domain

import amf.core.metamodel.domain.DomainElementModel._
import amf.core.model.BoolField
import amf.core.model.domain.extensions.DomainExtension

/**
  * Internal model for any domain element
  */
trait DomainElement extends AmfObject {

  def customDomainProperties: Seq[DomainExtension] = fields.field(CustomDomainProperties)
  def extend: Seq[DomainElement]                   = fields.field(Extends)

  def withCustomDomainProperties(extensions: Seq[DomainExtension]): this.type =
    setArray(CustomDomainProperties, extensions)

  def withCustomDomainProperty(extensions: DomainExtension): this.type =
    add(CustomDomainProperties, extensions)

  def isExternalLink: BoolField = fields.field(IsExternalLink)
  def withIsExternalLink(isReference: Boolean): DomainElement.this.type = set(IsExternalLink, isReference)

  def withExtends(extend: Seq[DomainElement]): this.type = setArray(Extends, extend)

  lazy val graph: Graph = Graph(this)

}
