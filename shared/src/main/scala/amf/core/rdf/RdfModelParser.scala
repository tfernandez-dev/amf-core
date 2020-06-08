package amf.core.rdf

import amf.core.metamodel.document.BaseUnitModel
import amf.core.model.document._
import amf.core.model.domain._
import amf.core.parser.errorhandler.ParserErrorHandler
import amf.core.plugin.PluginContext
import amf.core.rdf.graph.NodeFinder
import amf.core.rdf.helper.PluginEntitiesFacade
import amf.core.rdf.parsers._
import amf.plugins.features.validation.CoreValidations.UnableToParseRdfDocument

object RdfModelParser {
  def apply(errorHandler: ParserErrorHandler, plugins: PluginContext = PluginContext()): RdfModelParser =
    new RdfModelParser()(new RdfParserContext(eh = errorHandler, plugins = plugins))
}

class RdfModelParser()(implicit val ctx: RdfParserContext) extends RdfParserCommon {

  def parse(model: RdfModel, location: String): BaseUnit = {
    val unit = model.findNode(location) match {
      case Some(rootNode) =>
        // assumes root is always an Obj
        val nodeFinder = new NodeFinder(model)
        val parser = new ObjectParser(location,
                                      new RecursionControl(),
                                      new PluginEntitiesFacade(ctx),
                                      nodeFinder,
                                      new SourcesRetriever(nodeFinder))
        parser.parse(rootNode, findBaseUnit = true) match {
          case Some(unit: BaseUnit) =>
            unit.set(BaseUnitModel.Location, location.split("#").head)
            unit.withRunNumber(ctx.parserRun)
            unit
          case _ =>
            ctx.eh.violation(UnableToParseRdfDocument,
                             location,
                             s"Unable to parse RDF model for location root node: $location")
            Document()
        }
      case _ =>
        ctx.eh.violation(UnableToParseRdfDocument,
                         location,
                         s"Unable to parse RDF model for location root node: $location")
        Document()
    }

    // Resolve annotations after parsing entire graph
    ctx.collected.collect({ case r: ResolvableAnnotation => r }) foreach (_.resolve(ctx.nodes))
    unit
  }
}

class RecursionControl(private var visited: Set[String] = Set()) {
  def visited(node: Node): Unit = {
    this.visited = visited + node.subject
  }
  def hasVisited(node: Node): Boolean               = visited.contains(node.subject)
  def hasVisited(property: PropertyObject): Boolean = visited.contains(property.value)
}
