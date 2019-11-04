package amf.client.model.document

import amf.core.model.document.{ExternalFragment => InternalExternalFragment}

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
@JSExportTopLevel("ExternalFragment")
case class ExternalFragment(override private[amf] val _internal: InternalExternalFragment)
    extends Fragment(_internal) {

  @JSExportTopLevel("ExternalFragment")
  def this() = this(InternalExternalFragment())
}
