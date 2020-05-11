package amf.core.rdf

import amf.core.metamodel.Type.{Any, Array, Iri, Scalar, SortedArray, Str}
import amf.core.metamodel.document.{BaseUnitModel, DocumentModel}
import amf.core.metamodel.domain._
import amf.core.metamodel.{Field, Obj, Type}
import amf.core.model.document._
import amf.core.model.domain._
import amf.core.parser.Annotations
import amf.core.parser.errorhandler.ParserErrorHandler
import amf.core.plugin.PluginContext
import amf.core.rdf.converter.{AnyTypeConverter, ScalarTypeConverter, StringIriUriRegexParser}
import amf.core.rdf.graph.NodeFinder
import amf.core.rdf.helper.DefaultNodeClassSorter
import amf.core.rdf.parsers._
import amf.core.vocabulary.Namespace
import amf.plugins.features.validation.CoreValidations.{UnableToParseNode, UnableToParseRdfDocument}

import scala.collection.mutable.ListBuffer

object RdfModelParser {
  def apply(errorHandler: ParserErrorHandler, plugins: PluginContext = PluginContext()): RdfModelParser =
    new RdfModelParser()(new RdfParserContext(eh = errorHandler, plugins = plugins))
}

class RdfModelParser()(implicit val ctx: RdfParserContext) extends RdfParserCommon {

  private var graph: Option[RdfModel] = None
  private val sorter                  = new DefaultNodeClassSorter()
  private val recursionControl = new RecursionControl()
  private var rootId: Option[String] = None

  def parse(model: RdfModel, location: String): BaseUnit = {
    graph = Some(model)
    rootId = Some(location)

    val unit = model.findNode(location) match {
      case Some(rootNode) =>
        parsePossibleRecursion(rootNode, findBaseUnit = true) match {
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

  def parsePossibleRecursion(node: Node, findBaseUnit: Boolean = false): Option[AmfElement] = {
    if (recursionControl.hasVisited(node) && !isSelfEncoded(node)) ctx.nodes.get(node.subject)
    else {
      recursionControl.visited(node)
      parse(node, findBaseUnit)
    }
  }

  private def isSelfEncoded(node: Node) = node.subject == rootId.getOrElse("")

  private def parse(node: Node, findBaseUnit: Boolean = false): Option[AmfObject] = {
    val id = node.subject
    retrieveType(id, node, findBaseUnit) map { model =>
      val sources  = retrieveSources(node)
      val instance = buildType(model)(annots(sources, id))
      instance.withId(id)

      ctx.nodes = ctx.nodes + (id -> instance)

      checkLinkables(instance)
      // workaround for lazy values in shape
      val modelFields = extractModelFields(model)

      modelFields.foreach(f => {
        val k          = f.value.iri()
        val properties = key(node, k)
        traverse(instance, f, properties, sources, k)
      })

      // parsing custom extensions
      instance match {
        case l: DomainElement with Linkable => parseLinkableProperties(node, l)
        case ex: ExternalDomainElement if ctx.unresolvedExtReferencesMap.contains(ex.id) =>
          ctx.unresolvedExtReferencesMap.get(ex.id).foreach { element =>
            ex.raw.option().foreach(element.set(ExternalSourceElementModel.Raw, _))
          }
          ctx.unresolvedExtReferencesMap.remove(ex.id)
        case _ => // ignore
      }
      instance match {
        case elm: DomainElement =>
          new CustomPropertiesParser(new NodeFinder(graph.get), new SourcesRetriever(new NodeFinder(graph.get)))
            .parse(node, elm)
        case _ => // ignore
      }
      instance
    }
  }

  private def extractModelFields(model: Obj) = {
    model match {
      case shapeModel: ShapeModel =>
        shapeModel.fields ++ Seq(
          ShapeModel.CustomShapePropertyDefinitions,
          ShapeModel.CustomShapeProperties
        )
      case _ => model.fields
    }
  }

  protected def key(node: Node, property: String): Seq[PropertyObject] =
    node.getProperties(property).getOrElse(Nil)

  private def parseLinkableProperties(node: Node, instance: DomainElement with Linkable): Unit = {
    node
      .getProperties(LinkableElementModel.TargetId.value.iri())
      .flatMap(entries => {
        entries.headOption match {
          case Some(Uri(id)) => Some(id)
          case _             => None
        }
      })
      .foreach { targetId =>
        setLinkTarget(instance, targetId)
      }

    node
      .getProperties(LinkableElementModel.Label.value.iri())
      .flatMap(entries => {
        entries.headOption match {
          case Some(Literal(v, _)) => Some(v)
          case _                   => None
        }
      })
      .foreach(s => instance.withLinkLabel(s))
  }

  private def setLinkTarget(instance: DomainElement with Linkable, targetId: String) = {
    ctx.referencesMap.get(targetId) match {
      case Some(target) => instance.withLinkTarget(target)
      case None =>
        val unresolved: Seq[DomainElement] = ctx.unresolvedReferences.getOrElse(targetId, Nil)
        ctx.unresolvedReferences += (targetId -> (unresolved ++ Seq(instance)))
    }
  }

  private def traverse(instance: AmfObject,
                       f: Field,
                       properties: Seq[PropertyObject],
                       sources: SourceMap,
                       key: String): Unit = {
    if (properties.isEmpty) return
    val property = properties.head
    f.`type` match {
      case DataNodeModel => // dynamic nodes parsed here
        parseDynamicType(property) match {
          case Some(parsed) => instance.set(f, parsed, annots(sources, key))
          case _            =>
        }
      case _: Obj =>
        findLink(property) match {
          case Some(node) =>
            parsePossibleRecursion(node) match {
              case Some(parsed) =>
                instance.set(f, parsed, annots(sources, key))
              case _ => // ignore
            }
          case _ =>
            ctx.eh.violation(
              UnableToParseRdfDocument,
              instance.id,
              s"Error parsing RDF graph node, unknown linked node for property $key in node ${instance.id}")
        }

      case array: SortedArray if properties.length == 1 =>
        parseList(instance, array, f, properties, annots(sources, key))
      case _: SortedArray =>
        ctx.eh.violation(
          UnableToParseRdfDocument,
          instance.id,
          s"Error, more than one sorted array values found in node for property $key in node ${instance.id}")
      case a: Array =>
        val items = properties
        val values: Seq[AmfElement] = a.element match {
          case _: Obj =>
            val shouldParseUnit = f.value.iri() == (Namespace.Document + "references")
              .iri() // this is for self-encoded documents
            items.flatMap(n =>
              findLink(n) match {
                case Some(o) => parsePossibleRecursion(o, shouldParseUnit)
                case _       => None
            })
          case Str | Iri => items.map(StringIriUriRegexParser().parse)
        }
        instance.setArrayWithoutId(f, values, annots(sources, key))
      case _: Scalar => parseScalar(instance, f, property, annots(sources, key))
      case Any       => parseAny(instance, f, property, annots(sources, key))
    }

  }

  def parseDynamicType(id: PropertyObject): Option[DataNode] =
    new DynamicTypeParser(new NodeFinder(graph.get), new SourcesRetriever(new NodeFinder(graph.get))).parse(id)

  private def parseList(instance: AmfObject,
                        l: SortedArray,
                        field: Field,
                        properties: Seq[PropertyObject],
                        annotations: Annotations): Unit =
    instance.setArray(field, parseList(l.element, findLink(properties.head)), annotations)

  private def parseScalar(instance: AmfObject, field: Field, property: PropertyObject, annotations: Annotations): Unit =
    ScalarTypeConverter(field.`type`, property)(ctx.eh).tryConvert().foreach(instance.set(field, _, annotations))

  private def parseAny(instance: AmfObject, field: Field, property: PropertyObject, annotations: Annotations): Unit =
    AnyTypeConverter(property)(ctx.eh).tryConvert().foreach(instance.set(field, _, annotations))

  private def parseList(listElement: Type, maybeCollection: Option[Node]): Seq[AmfElement] = {
    val properties = getRdfProperties(maybeCollection)

    val res = properties.map { property =>
      listElement match {
        case DataNodeModel => parseDynamicType(property)
        case _: Obj =>
          findLink(property) match {
            case Some(node) => parsePossibleRecursion(node)
            case _          => None
          }
        case _: Scalar => ScalarTypeConverter(listElement, property)(ctx.eh).tryConvert()
        case Any       => AnyTypeConverter(property)(ctx.eh).tryConvert()
        case _         => throw new Exception(s"Unknown list element type: $listElement")
      }
    }
    res collect { case Some(x) => x }
  }

  private def getRdfProperties(maybeCollection: Option[Node]) = {
    val properties = ListBuffer[PropertyObject]()
    maybeCollection.foreach { collection =>
      collection.getKeys().foreach { entry =>
        if (entry.startsWith((Namespace.Rdfs + "_").iri())) {
          properties ++= collection.getProperties(entry).get
        }
      }
    }
    properties
  }

  private def findLink(property: PropertyObject) = new NodeFinder(graph.get).findLink(property)

  private def checkLinkables(instance: AmfObject): Unit = {
    instance match {
      case link: DomainElement with Linkable =>
        ctx.referencesMap += (link.id -> link)
        ctx.unresolvedReferences.getOrElse(link.id, Nil).foreach {
          case unresolved: Linkable => unresolved.withLinkTarget(link)
          case unresolved: LinkNode => unresolved.withLinkedDomainElement(link)
          case _                    => throw new Exception("Only linkable elements can be linked")
        }
        ctx.unresolvedReferences.update(link.id, Nil)
      case ref: ExternalSourceElement =>
        ctx.unresolvedExtReferencesMap += (ref.referenceId.value -> ref) // process when parse the references node
      case _ => // ignore
    }
  }

  private def isUnitModel(typeModel: Obj): Boolean =
    typeModel.isInstanceOf[DocumentModel] || typeModel.isInstanceOf[EncodesModel] || typeModel
      .isInstanceOf[DeclaresModel] || typeModel.isInstanceOf[BaseUnitModel]

  private def retrieveType(id: String, node: Node, findBaseUnit: Boolean = false): Option[Obj] = {
    val types = sorter.sortedClassesOf(node)

    val foundType = types.find { t =>
      val maybeFoundType = findType(t)
      // this is just for self-encoding documents
      maybeFoundType match {
        case Some(typeModel) if !findBaseUnit && !isUnitModel(typeModel) => true
        case Some(typeModel) if findBaseUnit && isUnitModel(typeModel)   => true
        case _                                                           => false
      }
    } orElse {
      // if I cannot find it, I will return the matching one directly, this is used
      // in situations where the references a reified, for example, in the canonical web api spec
      types.find(findType(_).isDefined)
    }

    foundType match {
      case Some(t) => findType(t)
      case None =>
        ctx.eh.violation(UnableToParseNode,
                         id,
                         s"Error parsing JSON-LD node, unknown @types $types",
                         ctx.rootContextDocument)
        None
    }
  }

  private def findType(`type`: String): Option[Obj]            = ctx.plugins.findType(`type`)
  private def buildType(`type`: Obj): Annotations => AmfObject = ctx.plugins.buildType(`type`)
  protected def retrieveSources(node: Node): SourceMap         = new SourcesRetriever(new NodeFinder(graph.get)).retrieve(node)
}

private class RecursionControl(private var visited: Set[String] = Set()) {
  def visited(node: Node): Unit = {
    this.visited = visited + node.subject
  }
  def hasVisited(node: Node): Boolean = visited.contains(node.subject)
  def hasVisited(property: PropertyObject): Boolean = visited.contains(property.value)
}