package amf.client.model.domain

import amf.client.convert.CoreClientConverters._
import amf.core.model.domain.extensions.{ShapeExtension => InternalShapeExtension}

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
@JSExportTopLevel("model.domain.ShapeExtension")
case class ShapeExtension(override private[amf] val _internal: InternalShapeExtension) extends DomainElement {

  @JSExportTopLevel("model.domain.ShapeExtension")
  def this() = this(InternalShapeExtension())

  def definedBy: PropertyShape = _internal.definedBy
  def extension: DataNode      = _internal.extension

  def withDefinedBy(definedBy: PropertyShape): this.type = {
    _internal.withDefinedBy(definedBy)
    this
  }

  def withExtension(extension: DataNode): this.type = {
    _internal.withExtension(extension)
    this
  }
}
