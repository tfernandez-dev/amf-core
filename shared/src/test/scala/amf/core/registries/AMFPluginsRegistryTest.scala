package amf.core.registries

import amf.plugins.document.graph.AMFGraphPlugin
import org.scalatest.FunSuite

class AMFPluginsRegistryTest extends FunSuite{


  test("test register and unregister document plugin"){

    AMFPluginsRegistry.registerDocumentPlugin(AMFGraphPlugin)

    assert(AMFPluginsRegistry.documentPluginForID(AMFGraphPlugin.ID).isDefined)

    assert(AMFPluginsRegistry.documentPluginForMediaType(AMFGraphPlugin.documentSyntaxes.head).nonEmpty)
    assert(AMFPluginsRegistry.documentPluginForVendor(AMFGraphPlugin.vendors.head).nonEmpty)

    AMFPluginsRegistry.unregisterDocumentPlugin(AMFGraphPlugin)

    assert(AMFPluginsRegistry.documentPluginForID(AMFGraphPlugin.ID).isEmpty)

    assert(AMFPluginsRegistry.documentPluginForMediaType(AMFGraphPlugin.documentSyntaxes.head).isEmpty)
    assert(AMFPluginsRegistry.documentPluginForVendor(AMFGraphPlugin.vendors.head).isEmpty)
  }
}
