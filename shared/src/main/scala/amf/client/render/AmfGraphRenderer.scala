package amf.client.render

import amf.client.convert.CoreClientConverters.ClientFuture
import amf.client.environment.Environment
import amf.client.model.document.BaseUnit
import amf.core.registries.AMFPluginsRegistry
import amf.plugins.document.graph.AMFGraphPlugin
import org.yaml.builder.DocBuilder

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
  * Amf generator.
  */
@JSExportAll
class AmfGraphRenderer private (private val env: Option[Environment])
    extends Renderer("AMF Graph", "application/ld+json", env) {

  @JSExportTopLevel("AmfGraphRenderer")
  def this() = this(None)
  @JSExportTopLevel("AmfGraphRenderer")
  def this(env: Environment) = this(Some(env))

  AMFPluginsRegistry.registerDocumentPlugin(AMFGraphPlugin)

  /** Asynchronously renders the syntax to a provided builder and returns it. */
  def generateToBuilder[T](unit: BaseUnit, builder: DocBuilder[T]): ClientFuture[Unit] =
    generateToBuilder(unit, RenderOptions(), builder)

  /** Asynchronously renders the syntax to a provided builder and returns it. */
  def generateToBuilder[T](unit: BaseUnit, options: RenderOptions, builder: DocBuilder[T]): ClientFuture[Unit] =
    genToBuilder(unit, options, builder)
}
