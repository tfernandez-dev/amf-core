package amf.client.model.document

import amf.core.model.document.{Module => InternalModule}

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
  * Module model class
  */
@JSExportAll
@JSExportTopLevel("Module")
case class Module(private[amf] val _internal: InternalModule) extends BaseUnit with DeclaresModel {

  @JSExportTopLevel("Module")
  def this() = this(InternalModule())
}
