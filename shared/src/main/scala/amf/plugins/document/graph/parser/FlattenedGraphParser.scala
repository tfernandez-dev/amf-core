package amf.plugins.document.graph.parser
import amf.client.parse.{DefaultParserErrorHandler, IgnoringErrorHandler}
import amf.core.annotations.DomainExtensionAnnotation
import amf.core.metamodel.Type.{
  Array,
  Bool,
  Iri,
  LiteralUri,
  RegExp,
  SortedArray,
  Str
}
import amf.core.metamodel.document.{BaseUnitModel, DocumentModel}
import amf.core.metamodel.domain.extensions.DomainExtensionModel
import amf.core.metamodel.domain.{
  DomainElementModel,
  ExternalSourceElementModel,
  LinkableElementModel,
  ShapeModel
}
import amf.core.metamodel.{Field, ModelDefaultBuilder, Obj, Type}
import amf.core.model.DataType
import amf.core.model.document._
import amf.core.model.domain._
import amf.core.model.domain.extensions.{CustomDomainProperty, DomainExtension}
import amf.core.parser._
import amf.core.registries.AMFDomainRegistry
import amf.core.remote.Platform
import amf.core.unsafe.TrunkPlatform
import amf.core.vocabulary.Namespace
import amf.plugins.features.validation.CoreValidations.{
  NodeNotFound,
  NotLinkable,
  UnableToParseDocument,
  UnableToParseNode
}
import org.mulesoft.common.time.SimpleDateTime
import org.yaml.model._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

class FlattenedGraphParser()(implicit val ctx: GraphParserContext)
    extends GraphParser {

  override def canParse(document: SyamlParsedDocument): Boolean = FlattenedGraphParser.canParse(document)

  override def parse(document: YDocument, location: String): BaseUnit = {
    val parser = Parser(Map())
    parser.parse(document, location)
  }

  case class Parser(var nodes: Map[String, AmfElement]) extends GraphParserHelpers {
    private val unresolvedReferences = mutable.Map[String, Seq[DomainElement]]()
    private val unresolvedExtReferencesMap =
      mutable.Map[String, ExternalSourceElement]()

    private val referencesMap                           = mutable.Map[String, DomainElement]()
    private val cache                                   = mutable.Map[String, AmfObject]()
    private val graphMap: mutable.HashMap[String, YMap] = mutable.HashMap.empty

    def parse(document: YDocument, location: String): BaseUnit = {
      document.node.value match {
        case documentMap: YMap =>
          documentMap.key("@context").foreach(e => parseCompactUris(e.value))
          documentMap.key("@graph").flatMap { e =>
            parseGraph(e.value)
          } match {
            case Some(b: BaseUnit) =>
              b.withLocation(location)
            case _ =>
              ctx.eh.violation(UnableToParseDocument, "", "Error parsing root JSON-LD node")
              Document() // TODO returning empty document on failure
          }
        case _ =>
          ctx.eh.violation(UnableToParseDocument, "", "Error parsing root JSON-LD node")
          Document()
      }
    }

    private def parseGraph(graph: YNode): Option[BaseUnit] = {
      populateGraphMap(graph.as[YSequence])
      getRootNodeFromGraphMap match {
        case Some(rootNode) =>
          parse(rootNode) match {
            case Some(b: BaseUnit) =>
              Some(b)
            case Some(_) =>
              ctx.eh.violation(UnableToParseDocument, "", "Root node is not a Base Unit")
              None
            case _ =>
              ctx.eh.violation(UnableToParseDocument, "", "Unable to parse root node")
              None
          }

        case None =>
          ctx.eh.violation(UnableToParseDocument, "", "Cannot find root node for flattened JSON-LD")
          None
      }
    }

    private def populateGraphMap(graphNodes: YSequence): Unit = {
      def toMapEntry(node: YNode): Option[(String, YMap)] = {
        val map = node.as[YMap] // All nodes at root level should be objects
        retrieveId(map, ctx) match {
          case Some(id) => Some((id, map))
          case _        => None
        }
      }
      graphNodes.nodes.flatMap { toMapEntry }.foreach {
        case (id, map) => graphMap(id) = map
      }
    }

    private def getRootNodeFromGraphMap: Option[YMap] = {
      graphMap.values.find { node =>
        node.entries
          .find { entry =>
            expandUriFromContext(entry.key.as[String]) == BaseUnitModel.Root.value.iri()
          }
          .exists(entry => entry.value.as[Boolean])
      }
    }

    private def retrieveType(id: String, map: YMap): Option[Obj] = {
      val stringTypes = ts(map, id)
      stringTypes.find(findType(_).isDefined) match {
        case Some(t) => findType(t)
        case None =>
          ctx.eh.violation(UnableToParseNode, id, s"Error parsing JSON-LD node, unknown @types $stringTypes", map)
          None
      }
    }

    private def parseSortedArray(listElement: Type, rawNode: YMap): Seq[AmfElement] = {
      def key(entry: YMapEntry): String = entry.key.as[String]
      contentOfNode(rawNode) match {
        case Some(node) =>
          // Sorted array members
          val members = node.entries.filter { entry =>
            val property           = key(entry)
            val sortedMemberPrefix = (Namespace.Rdfs + "_").iri()
            property.startsWith(compactUriFromContext(sortedMemberPrefix))
          }

          // Parse members
          members.sortBy(key).flatMap { entry =>
            listElement match {
              case _: Obj => parse(entry.value.as[YMap])
              case _ =>
                try { Some(str(value(listElement, entry.value))) } catch {
                  case _: Exception => None
                }
            }
          }
        case None => Seq.empty // Error already handled by contentOfNode
      }
    }

    private def parse(map: YMap): Option[AmfObject] = { // todo fix uses
      if (isJsonLdLink(map)) {
        retrieveId(map, ctx).flatMap { id =>
          cache.get(id) match {
            case Some(parsed) => Some(parsed)
            case None         =>
              // Cache miss
              nodeFromId(id).flatMap(parse)
          }
        }

      }
      else {
        retrieveId(map, ctx)
          .flatMap(value => retrieveType(value, map).map(value2 => (value, value2)))
          .flatMap {
            case (id, model) =>
              val sources               = retrieveSources(id, map)
              val transformedId: String = transformIdFromContext(id)
              buildType(transformedId, map, model)(annotations(nodes, sources, transformedId)) match {
                case Some(builder) =>
                  val instance: AmfObject = builder
                  instance.withId(transformedId)

                  checkLinkables(instance)

                  // workaround for lazy values in shape
                  val modelFields = model match {
                    case shapeModel: ShapeModel =>
                      shapeModel.fields ++ Seq(
                          ShapeModel.CustomShapePropertyDefinitions,
                          ShapeModel.CustomShapeProperties
                      )
                    case _ => model.fields
                  }

                  modelFields.foreach(f => {
                    val k = compactUriFromContext(f.value.iri())
                    map.key(k) match {
                      case Some(entry) =>
                        traverse(instance, f, value(f.`type`, entry.value), sources, k)
                      case _ =>
                    }
                  })

                  // parsing custom extensions
                  instance match {
                    case l: DomainElement with Linkable =>
                      parseLinkableProperties(map, l)
                    case ex: ExternalDomainElement if unresolvedExtReferencesMap.get(ex.id).isDefined =>
                      unresolvedExtReferencesMap.get(ex.id).foreach { element =>
                        ex.raw
                          .option()
                          .foreach(element.set(ExternalSourceElementModel.Raw, _))
                      }
                    case obj: ObjectNode =>
                      parseObjectNodeProperties(obj, map, modelFields)

                    case _ => // ignore
                  }
                  instance match {
                    case elm: DomainElement => parseCustomProperties(map, elm)
                    case _                  => // ignore
                  }

                  nodes = nodes + (transformedId -> instance)
                  Some(instance)
                case _ => None
              }

          }
      }

    }

    override protected def contentOfNode(n: YNode): Option[YMap] = {
      val id: Option[String] = n.asOption[YMap].flatMap(retrieveId(_,ctx))
      id.flatMap(nodeFromId)
    }

    private def nodeFromId(id: String): Option[YMap] = {
      graphMap.get(id) match {
        case Some(raw) => Some(raw)
        case None =>
          ctx.eh.violation(UnableToParseDocument, "", s"Cannot find node with $id")
          None
      }
    }

    private def checkLinkables(instance: AmfObject): Unit = {
      instance match {
        case link: DomainElement with Linkable =>
          referencesMap += (link.id -> link)
          unresolvedReferences.getOrElse(link.id, Nil).foreach {
            case unresolved: Linkable =>
              unresolved.withLinkTarget(link)
            case unresolved: LinkNode =>
              unresolved.withLinkedDomainElement(link)
            case _ =>
              ctx.eh.violation(NotLinkable, instance.id, "Only linkable elements can be linked", instance.annotations)
          }
          unresolvedReferences.update(link.id, Nil)
        case ref: ExternalSourceElement =>
          unresolvedExtReferencesMap += (ref.referenceId.value -> ref) // process when parse the references node
        case _ => // ignore
      }
    }

    private def setLinkTarget(instance: DomainElement with Linkable, targetId: String) = {
      referencesMap.get(targetId) match {
        case Some(target) => instance.withLinkTarget(target)
        case None =>
          val unresolved: Seq[DomainElement] =
            unresolvedReferences.getOrElse(targetId, Nil)
          unresolvedReferences += (targetId -> (unresolved ++ Seq(instance)))
      }
    }

    private def parseLinkableProperties(map: YMap, instance: DomainElement with Linkable): Unit = {
      map
        .key(compactUriFromContext(LinkableElementModel.TargetId.value.iri()))
        .flatMap(entry => {
          retrieveId(entry.value.as[Seq[YMap]].head, ctx)
        })
        .foreach { targetId =>
          val transformedId = transformIdFromContext(targetId)
          setLinkTarget(instance, transformedId)
        }

      map
        .key(compactUriFromContext(LinkableElementModel.Label.value.iri()))
        .flatMap(entry => {
          entry.value
            .toOption[Seq[YNode]]
            .flatMap(nodes => nodes.head.toOption[YMap])
            .flatMap(map => map.key("@value"))
            .flatMap(_.value.toOption[YScalar].map(_.text))
        })
        .foreach(s => instance.withLinkLabel(s))
    }

    private def parseCustomProperties(map: YMap, instance: DomainElement): Unit = {
      val properties = map
        .key(compactUriFromContext(DomainElementModel.CustomDomainProperties.value.iri()))
        .map(_.value.as[Seq[YNode]].map(value(Iri, _).as[YScalar].text))
        .getOrElse(Nil)

      val extensions = properties
        .flatMap { uri =>
          map
            .key(compactUriFromContext(uri))
            .map(entry => {
              val extension = DomainExtension()
              val obj       = entry.value.as[YMap]

              parseScalarProperty(obj, DomainExtensionModel.Name)
                .map(extension.withName)
              parseScalarProperty(obj, DomainExtensionModel.Element)
                .map(extension.withElement)

              val definition = CustomDomainProperty()
              definition.id = uri
              extension.withDefinedBy(definition)

              parse(obj).collect({ case d: DataNode => d }).foreach { pn =>
                extension.withId(pn.id)
                extension.withExtension(pn)
              }

              val sources = retrieveSources(extension.id, map)
              extension.annotations ++= annotations(nodes, sources, extension.id)

              extension
            })
        }

      if (extensions.nonEmpty) {
        extensions.partition(_.isScalarExtension) match {
          case (scalars, objects) =>
            instance.withCustomDomainProperties(objects)
            applyScalarDomainProperties(instance, scalars)
        }
      }
    }

    private def applyScalarDomainProperties(instance: DomainElement, scalars: Seq[DomainExtension]): Unit = {
      scalars.foreach { e =>
        instance.fields
          .fieldsMeta()
          .find(f => e.element.is(f.value.iri()))
          .foreach(f => {
            instance.fields.entry(f).foreach {
              case FieldEntry(_, value) =>
                value.value.annotations += DomainExtensionAnnotation(e)
            }
          })
      }
    }

    private def parseObjectNodeProperties(obj: ObjectNode, map: YMap, fields: List[Field]): Unit = {
      map.entries.foreach { entry =>
        val uri = expandUriFromContext(entry.key.as[String])
        val v   = entry.value
        if (uri != "@type" && uri != "@id" && uri != DomainElementModel.Sources.value.iri() && uri != "smaps" &&
            uri != (Namespace.Document + "name")
              .iri() && !fields.exists(_.value.iri() == uri)) { // we do this to prevent parsing name of annotations
          v.as[Seq[YMap]]
            .headOption
            .flatMap(parse)
            .collect({ case d: amf.core.model.domain.DataNode => obj.addProperty(uri, d) })
        }
      }
    }

    private def isJsonLdLink(m: YMap): Boolean = m.entries.size == 1 && m.key("@id").isDefined

    private def traverse(instance: AmfObject, f: Field, node: YNode, sources: SourceMap, key: String) = {
      f.`type` match {
        case _: Obj =>
          parse(node.as[YMap]).foreach(n => instance.set(f, n, annotations(nodes, sources, key)))
          instance
        case Str | RegExp | Iri | LiteralUri =>
          instance.set(f, str(node), annotations(nodes, sources, key))
        case Bool =>
          instance.set(f, bool(node), annotations(nodes, sources, key))
        case Type.Int =>
          instance.set(f, int(node), annotations(nodes, sources, key))
        case Type.Float =>
          instance.set(f, float(node), annotations(nodes, sources, key))
        case Type.Double =>
          instance.set(f, double(node), annotations(nodes, sources, key))
        case Type.DateTime =>
          instance.set(f, date(node), annotations(nodes, sources, key))
        case Type.Date =>
          instance.set(f, date(node), annotations(nodes, sources, key))
        case Type.Any =>
          instance.set(f, any(node), annotations(nodes, sources, key))
        case l: SortedArray =>
          val parsed = parseSortedArray(l.element, node.as[YMap])
          instance.setArray(f, parsed, annotations(nodes, sources, key))
        case a: Array =>
          val items = node.as[Seq[YNode]]
          val values: Seq[AmfElement] = a.element match {
            case _: Obj    => items.flatMap(n => parse(n.as[YMap]))
            case Str | Iri => items.map(n => str(value(a.element, n)))
          }
          a.element match {
            case _: DomainElementModel if f == DocumentModel.Declares =>
              instance.setArrayWithoutId(f, values, annotations(nodes, sources, key))
            case _: BaseUnitModel =>
              instance.setArrayWithoutId(f, values, annotations(nodes, sources, key))
            case _ =>
              instance.setArray(f, values, annotations(nodes, sources, key))
          }
      }
    }

    private def parseScalarProperty(definition: YMap, field: Field) =
      definition
        .key(compactUriFromContext(field.value.iri()))
        .map(entry => value(field.`type`, entry.value).as[YScalar].text)

    private def str(node: YNode) = {
      val value = node.tagType match {
        case YType.Map =>
          node.as[YMap].entries.find(_.key.as[String] == "@value") match {
            case Some(entry) => entry.value.as[YScalar].text
            case _           => node.as[YScalar].text
          }
        case _ => node.as[YScalar].text
      }
      AmfScalar(value)
    }

    private def bool(node: YNode) = {
      val value = node.tagType match {
        case YType.Map =>
          node.as[YMap].entries.find(_.key.as[String] == "@value") match {
            case Some(entry) => entry.value.as[YScalar].text.toBoolean
            case _           => node.as[YScalar].text.toBoolean
          }
        case _ => node.as[YScalar].text.toBoolean
      }
      AmfScalar(value)
    }

    private def int(node: YNode) = {
      val value = node.tagType match {
        case YType.Map =>
          node.as[YMap].entries.find(_.key.as[String] == "@value") match {
            case Some(entry) => entry.value.as[YScalar].text.toInt
            case _           => node.as[YScalar].text.toInt
          }
        case _ => node.as[YScalar].text.toInt
      }
      AmfScalar(value)
    }

    private def double(node: YNode) = {
      val value = node.tagType match {
        case YType.Map =>
          node.as[YMap].entries.find(_.key.as[String] == "@value") match {
            case Some(entry) => entry.value.as[YScalar].text.toDouble
            case _           => node.as[YScalar].text.toDouble
          }
        case _ => node.as[YScalar].text.toDouble
      }
      AmfScalar(value)
    }

    private def date(node: YNode) = {
      val value = node.tagType match {
        case YType.Map =>
          node.as[YMap].entries.find(_.key.as[String] == "@value") match {
            case Some(entry) =>
              SimpleDateTime.parse(entry.value.as[YScalar].text).right.get
            case _ => SimpleDateTime.parse(node.as[YScalar].text).right.get
          }
        case _ => SimpleDateTime.parse(node.as[YScalar].text).right.get
      }
      AmfScalar(value)
    }

    private val xsdString: String   = DataType.String
    private val xsdInteger: String  = DataType.Integer
    private val xsdFloat: String    = DataType.Float
    private val amlNumber: String   = DataType.Number
    private val xsdDouble: String   = DataType.Double
    private val xsdBoolean: String  = DataType.Boolean
    private val xsdDateTime: String = DataType.DateTime
    private val xsdDate: String     = DataType.Date

    private def any(node: YNode) = {
      node.tagType match {
        case YType.Map =>
          val nodeValue =
            node.as[YMap].entries.find(_.key.as[String] == "@value") match {
              case Some(entry) => entry.value.as[YScalar].text
              case _           => node.as[YScalar].text
            }
          node.as[YMap].entries.find(_.key.as[String] == "@type") match {
            case Some(typeEntry) =>
              val typeUri     = typeEntry.value.as[YScalar].text
              val expandedUri = expandUriFromContext(typeUri)
              expandedUri match {
                case s: String if s == xsdBoolean =>
                  AmfScalar(nodeValue.toBoolean)
                case s: String if s == xsdInteger => AmfScalar(nodeValue.toInt)
                case s: String if s == xsdFloat   => AmfScalar(nodeValue.toFloat)
                case s: String if s == xsdDouble  => AmfScalar(nodeValue.toDouble)
                case s: String if s == xsdDateTime =>
                  AmfScalar(SimpleDateTime.parse(nodeValue).right.get)
                case s: String if s == xsdDate =>
                  AmfScalar(SimpleDateTime.parse(nodeValue).right.get)
                case _ => AmfScalar(nodeValue)
              }
            case _ => AmfScalar(nodeValue)
          }
        case _ => AmfScalar(node.as[YScalar].text)
      }
    }

    private def float(node: YNode) = {
      val value = node.tagType match {
        case YType.Map =>
          node.as[YMap].entries.find(_.key.as[String] == "@value") match {
            case Some(entry) =>
              entry.value.as[YScalar].text.toDouble
            case _ => node.as[YScalar].text.toDouble
          }
        case _ => node.as[YScalar].text.toDouble
      }
      AmfScalar(value)
    }

    private val types: Map[String, Obj] = Map.empty ++ AMFDomainRegistry.metadataRegistry

    private def findType(typeString: String): Option[Obj] = {
      types.get(expandUriFromContext(typeString)).orElse(AMFDomainRegistry.findType(typeString))
    }

    private def buildType(id: String, map: YMap, modelType: Obj): Annotations => Option[AmfObject] = {
      AMFDomainRegistry.metadataRegistry.get(modelType.`type`.head.iri()) match {
        case Some(modelType: ModelDefaultBuilder) =>
          (annotations: Annotations) =>
            val instance = modelType.modelInstance
            instance.annotations ++= annotations
            Some(instance)
        case _ =>
          AMFDomainRegistry.buildType(modelType) match {
            case Some(builder) =>
              (a: Annotations) =>
                Some(builder(a))
            case _ =>
              ctx.eh.violation(NodeNotFound, id, s"Cannot find builder for node type $modelType", map)
              (_: Annotations) =>
                None
          }
      }
    }
  }
}

object FlattenedGraphParser extends GraphContextHelper {
  def apply(): FlattenedGraphParser =
    new FlattenedGraphParser()(
      new GraphParserContext(eh = DefaultParserErrorHandler.withRun())
    )

  /**
    * Returns true if `document` contains a @graph node which in turn must contain a Root node.
    * Root nodes are nodes with @type http://a.ml/vocabularies/document#Unit (BaseUnits) with the property
    * http://a.ml/vocabularies/document#root set to true.
    * @param document document to perform the canParse test on
    * @return
    */
  def canParse(document: SyamlParsedDocument): Boolean = {
    document.document.node.value match {
      case m: YMap =>
        implicit val ctx: GraphParserContext = new GraphParserContext(
          eh = IgnoringErrorHandler()
        )
        m.key("@context").foreach(entry => parseCompactUris(entry.value))
        m.key("@graph") match {
          case Some(graphEntry) =>
            val graphYSeq = graphEntry.value.as[YSequence]
            graphYSeq.nodes.exists { node =>
              isRootNode(node)
            }
          case None => false
        }
      case _ => false
    }
  }

  private def isRootNode(node: YNode)(implicit ctx: GraphParserContext): Boolean = {
    node.value match {
      case map: YMap =>
        val isBaseUnit = map.key("@type").exists { entry =>
          val typesYSeq = entry.value.as[YSequence]
          typesYSeq.nodes
            .flatMap(_.asScalar)
            .exists(t => {
              BaseUnitModel.`type`
                .exists(_.iri() == expandUriFromContext(t.text))
            })
        }
        val isRoot = map.entries.exists(entry => {
          val key = expandUriFromContext(entry.key.as[String])
          BaseUnitModel.Root.value.iri() == key && entry.value.as[Boolean]
        })
        isBaseUnit && isRoot
      case _ => false
    }

  }
}
