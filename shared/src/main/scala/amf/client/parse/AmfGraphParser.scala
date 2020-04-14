package amf.client.parse

import amf.client.environment.Environment
import amf.core.registries.AMFPluginsRegistry
import amf.plugins.document.graph.AMFGraphPlugin

import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * Amf parser.
  */
class AmfGraphParser private (private val env: Option[Environment] = None)
    extends Parser("AMF Graph", "application/ld+json", env) {

  @JSExportTopLevel("AmfGraphParser")
  def this() = this(None)
  @JSExportTopLevel("AmfGraphParser")
  def this(environment: Environment) = this(Some(environment))

  AMFPluginsRegistry.registerDocumentPlugin(AMFGraphPlugin)
}