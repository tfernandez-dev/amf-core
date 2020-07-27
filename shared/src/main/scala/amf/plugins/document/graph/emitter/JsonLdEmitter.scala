package amf.plugins.document.graph.emitter

import amf.core.annotations._
import amf.core.emitter.RenderOptions
import amf.core.metamodel.Type.{Any, Array, Bool, EncodedIri, Iri, LiteralUri, SortedArray, Str}
import amf.core.metamodel._
import amf.core.metamodel.document.{ModuleModel, SourceMapModel}
import amf.core.metamodel.domain.extensions.DomainExtensionModel
import amf.core.metamodel.domain.{DomainElementModel, ExternalSourceElementModel, LinkableElementModel, ShapeModel}
import amf.core.model.DataType
import amf.core.model.document.{BaseUnit, SourceMap}
import amf.core.model.domain.DataNodeOps.adoptTree
import amf.core.model.domain._
import amf.core.model.domain.extensions.DomainExtension
import amf.core.parser.{Annotations, FieldEntry, Value}
import amf.core.vocabulary.{Namespace, ValueType}
import org.mulesoft.common.time.SimpleDateTime
import org.yaml.builder.DocBuilder
import org.yaml.builder.DocBuilder.{Entry, Part, SType, Scalar}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object JsonLdEmitter {

  def emit[T](unit: BaseUnit, builder: DocBuilder[T], renderOptions: RenderOptions = RenderOptions()): Boolean = {
    implicit val ctx: EmissionContext = EmissionContext(unit, renderOptions)
    new JsonLdEmitter[T](builder, renderOptions).root(unit)
    true
  }
}

class JsonLdEmitter[T](val builder: DocBuilder[T], val options: RenderOptions)(implicit ctx: EmissionContext)
    extends CommonEmitter
    with MetaModelTypeMapping {

  val cache: mutable.Map[String, T] = mutable.Map[String, T]()

  def root(unit: BaseUnit): Unit = {
    val declaresEntry: Option[FieldEntry]   = unit.fields.entry(ModuleModel.Declares)
    val referencesEntry: Option[FieldEntry] = unit.fields.entry(ModuleModel.References)

    extractDeclarationsAndReferencesToContext(declaresEntry, referencesEntry, unit.annotations)

    unit.fields.removeField(ModuleModel.Declares)
    unit.fields.removeField(ModuleModel.References)

    builder.list {
      _.obj { eb =>
        traverse(unit, eb)
        emitReferences(eb, unit.id, SourceMap(unit.id, unit))
        emitDeclarations(eb, unit.id, SourceMap(unit.id, unit))
        ctx.emitContext(eb)
      }
    }

    // Restore model previous version
    declaresEntry.foreach(e => unit.fields.setWithoutId(ModuleModel.Declares, e.array))
    referencesEntry.foreach(e => unit.fields.setWithoutId(ModuleModel.References, e.array))
  }

  private def emitReferences(b: Entry[T], id: String, sources: SourceMap): Unit = {
    if (ctx.referenced.nonEmpty) {
      val v   = Value(AmfArray(ctx.referenced), Annotations())
      val f   = ModuleModel.References
      val url = ctx.emitIri(f.value.iri())
      ctx.emittingReferences(true)
      b.entry(
          url,
          value(f.`type`, v, id, sources.property(url), _)
      )
    }
    ctx.emittingReferences(false)
  }

  private def emitDeclarations(b: Entry[T], id: String, sources: SourceMap): Unit = {
    if (ctx.declared.nonEmpty) {
      val v   = Value(AmfArray(ctx.declared), Annotations())
      val f   = ModuleModel.Declares
      val url = ctx.emitIri(f.value.iri())
      ctx.emittingDeclarations(true)
      b.entry(
          url,
          value(f.`type`, v, id, sources.property(url), _)
      )
    }
    ctx.emittingDeclarations(false)
  }

  def traverse(element: AmfObject, b: Entry[T]): Unit = {
    val id = element.id

    createIdNode(b, id)

    val sources = SourceMap(id, element)

    val obj = metaModel(element)

    traverseMetaModel(id, element, sources, obj, b)

    createCustomExtensions(element, b)

    val sourceMapId = sourceMapIdFor(id)
    createSourcesNode(sourceMapId, sources, b)
  }

  def traverseMetaModel(id: String, element: AmfObject, sources: SourceMap, obj: Obj, b: Entry[T]): Unit = {
    createTypeNode(b, obj)

    val modelFields = getMetaModelFields(element, obj)

    // no longer necessary?
    element match {
      case e: ObjectNode if options.isValidation =>
        val url = Namespace.AmfValidation.base + "/properties"
        b.entry(
            url,
            value(Type.Int, Value(AmfScalar(e.propertyFields().size), Annotations()), id, _ => {}, _)
        )
      case _ => // Nothing to do
    }

    modelFields.foreach { f =>
      emitStaticField(f, element, id, sources, b)
    }

  }

  private def emitStaticField(field: Field, element: AmfObject, id: String, sources: SourceMap, b: Entry[T]): Unit = {
    element.fields.entryJsonld(field) match {
      case Some(FieldEntry(f, v)) =>
        val url = ctx.emitIri(f.value.iri())
        b.entry(
            url,
            value(f.`type`, v, id, sources.property(url), _)
        )
      case None => // Missing field
    }
  }

  private def createCustomExtensions(element: AmfObject, b: Entry[T]): Unit = {
    val customProperties: ListBuffer[String] = ListBuffer()

    // Collect element custom annotations
    element.fields.entry(DomainElementModel.CustomDomainProperties) foreach {
      case FieldEntry(_, v) =>
        v.value match {
          case AmfArray(values, _) =>
            values
              .sortBy(_.asInstanceOf[DomainExtension].id)
              .foreach {
                case extension: DomainExtension =>
                  val uri = extension.definedBy.id
                  customProperties += uri
                  createCustomExtension(b, uri, extension, None)
              }
          case _ => // ignore
        }
    }

    // Collect element scalar fields custom annotations
    var count = 1
    element.fields.foreach {
      case (f, v) =>
        v.value.annotations
          .collect({ case e: DomainExtensionAnnotation => e })
          .sortBy(_.extension.id)
          .foreach(e => {
            val extension = e.extension
            val uri       = s"${element.id}/scalar-valued/$count/${extension.name.value()}"
            customProperties += uri
            adoptTree(uri, extension.extension) // Fix ids
            createCustomExtension(b, uri, extension, Some(f))
            count += 1
          })
    }

    if (customProperties.nonEmpty)
      b.entry(
          ctx.emitIri(DomainElementModel.CustomDomainProperties.value.iri()),
          _.list { b =>
            customProperties.foreach(iri(b, _, inArray = true))
          }
      )
  }

  private def createCustomExtension(b: Entry[T],
                                    uri: String,
                                    extension: DomainExtension,
                                    field: Option[Field] = None): Unit = {
    b.entry(
        uri,
        _.obj { b =>
          b.entry(
              ctx.emitIri(DomainExtensionModel.Name.value.iri()),
              listWithScalar(_, extension.name.value())
          )
          field.foreach(
              f =>
                b.entry(
                    ctx.emitIri(DomainExtensionModel.Element.value.iri()),
                    listWithScalar(_, f.value.iri())
              ))
          traverse(extension.extension, b)
        }
    )
  }

  def createSortedArray(b: Part[T],
                        seq: Seq[AmfElement],
                        parent: String,
                        element: Type): Unit = {
    b.list {
      _.obj { b =>
        val id = s"$parent/list"
        createIdNode(b, id)
        b.entry("@type", ctx.emitIri((Namespace.Rdfs + "Seq").iri()))
        seq.zipWithIndex.foreach {
          case (e, i) =>
            b.entry(
                ctx.emitIri((Namespace.Rdfs + s"_${i + 1}").iri()), { b =>
                  b.list {
                    b =>
                      element match {
                        case _: Obj =>
                          e match {
                            case elementInArray: DomainElement with Linkable if elementInArray.isLink =>
                              link(b, elementInArray, inArray = true)
                            case elementInArray: AmfObject =>
                              obj(b, elementInArray, inArray = true)
                          }
                        case Str =>
                          scalar(b, e, SType.Str)

                        case EncodedIri =>
                          safeIri(b, e.asInstanceOf[AmfScalar].toString, inArray = true)

                        case Iri =>
                          iri(b, e.asInstanceOf[AmfScalar].toString, inArray = true)

                        case Any =>
                          val scalarElement = e.asInstanceOf[AmfScalar]
                          scalarElement.value match {
                            case bool: Boolean =>
                              typedScalar(b, bool.toString, DataType.Boolean, inArray = true)
                            case str: String =>
                              typedScalar(b, str.toString, DataType.String, inArray = true)
                            case i: Int =>
                              typedScalar(b, i.toString, DataType.Integer, inArray = true)
                            case f: Float =>
                              typedScalar(b, f.toString, DataType.Float, inArray = true)
                            case d: Double =>
                              typedScalar(b, d.toString, DataType.Double, inArray = true)
                            case other => scalar(b, other.toString)
                          }
                      }
                  }
                }
            )
        }
      }
    }
  }

  private def value(t: Type, v: Value, parent: String, sources: Value => Unit, b: Part[T]): Unit = {
    t match {
      case _: ShapeModel if ctx.canGenerateLink(v.value) =>
        extractToLink(v.value.asInstanceOf[Shape], b)
      case t: DomainElement with Linkable if t.isLink =>
        link(b, t)
        sources(v)
      case _: Obj =>
        obj(b, v.value.asInstanceOf[AmfObject])
        sources(v)
      case Iri =>
        iri(b, v.value.asInstanceOf[AmfScalar].toString)
        sources(v)
      case EncodedIri =>
        safeIri(b, v.value.asInstanceOf[AmfScalar].toString)
        sources(v)
      case LiteralUri =>
        typedScalar(b, v.value.asInstanceOf[AmfScalar].toString, DataType.AnyUri)
        sources(v)
      case Str =>
        listWithScalar(b, v.value)
        sources(v)
      case Bool =>
        listWithScalar(b, v.value, SType.Bool)
        sources(v)
      case Type.Int =>
        listWithScalar(b, v.value, SType.Int)
        sources(v)
      case Type.Double =>
        // this will transform the value to double and will not emit @type TODO: ADD YType.Double
        listWithScalar(b, v.value, SType.Float)
        sources(v)
      case Type.Float =>
        listWithScalar(b, v.value, SType.Float)
        sources(v)
      case Type.DateTime =>
        val dateTime = v.value.asInstanceOf[AmfScalar].value.asInstanceOf[SimpleDateTime]
        typedScalar(b, dateTime.toString, DataType.DateTime)
        sources(v)
      case Type.Date =>
        val maybeDateTime = v.value.asInstanceOf[AmfScalar].value match {
          case dt: SimpleDateTime => Some(dt)
          case other              => SimpleDateTime.parse(other.toString).toOption
        }
        maybeDateTime match {
          case Some(dateTime) =>
            typedScalar(
                b,
                dateTime.toString,
                if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined) DataType.DateTime else DataType.Date)
          case _ =>
            listWithScalar(b, v.value)
        }
        sources(v)
      case a: SortedArray =>
        createSortedArray(b, v.value.asInstanceOf[AmfArray].values, parent, a.element)
        sources(v)
      case a: Array =>
        b.list { b =>
          val seq = v.value.asInstanceOf[AmfArray]
          sources(v)
          a.element match {
            case _: Obj =>
              seq.values.asInstanceOf[Seq[AmfObject]].foreach {
                case v @ (_: Shape) if ctx.canGenerateLink(v) =>
                  extractToLink(v.asInstanceOf[Shape], b, true)
                case elementInArray: DomainElement with Linkable if elementInArray.isLink =>
                  link(b, elementInArray, inArray = true)
                case elementInArray =>
                  obj(b, elementInArray, inArray = true)
              }
            case Str =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach { e =>
                scalar(b, e.toString)
              }
            case EncodedIri =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach(e => safeIri(b, e.toString, inArray = true))
            case Iri =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach(e => iri(b, e.toString, inArray = true))
            case LiteralUri =>
              typedScalar(b, v.value.asInstanceOf[AmfScalar].toString, DataType.AnyUri, inArray = true)
            case Type.Int =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach(e => scalar(b, e.value.toString, SType.Int))
            case Type.Float =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach(e => scalar(b, e.value.toString, SType.Float))
            case Bool =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach(e => scalar(b, e.value.toString, SType.Bool))
            case Type.DateTime =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach { e =>
                  val dateTime = e.value.asInstanceOf[SimpleDateTime]
                  typedScalar(b, dateTime.toString, DataType.DateTime)
                }
            case Type.Date =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach { e =>
                  val dateTime = e.value.asInstanceOf[SimpleDateTime]
                  emitSimpleDateTime(b, dateTime, inArray = false)
                }
            case Any =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach { scalarElement =>
                scalarElement.value match {
                  case bool: Boolean =>
                    typedScalar(b, bool.toString, DataType.Boolean, inArray = true)
                  case i: Int              => typedScalar(b, i.toString, DataType.Integer, inArray = true)
                  case f: Float            => typedScalar(b, f.toString, DataType.Float, inArray = true)
                  case d: Double           => typedScalar(b, d.toString, DataType.Double, inArray = true)
                  case sdt: SimpleDateTime => emitSimpleDateTime(b, sdt)
                  case other               => scalar(b, other.toString)
                }
              }
            case _ => seq.values.asInstanceOf[Seq[AmfScalar]].foreach(e => iri(b, e.toString, inArray = true))
          }
        }

      case Any if v.value.isInstanceOf[AmfScalar] =>
        v.value.asInstanceOf[AmfScalar].value match {
          case bool: Boolean       => typedScalar(b, bool.toString, DataType.Boolean, inArray = true)
          case i: Int              => typedScalar(b, i.toString, DataType.Integer, inArray = true)
          case f: Float            => typedScalar(b, f.toString, DataType.Float, inArray = true)
          case d: Double           => typedScalar(b, d.toString, DataType.Double, inArray = true)
          case sdt: SimpleDateTime => emitSimpleDateTime(b, sdt)
          case other               => scalar(b, other.toString)
        }
    }
  }

  private def emitSimpleDateTime(b: Part[T], dateTime: SimpleDateTime, inArray: Boolean = true): Unit = {
    if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined)
      typedScalar(b, dateTime.toString, DataType.DateTime, inArray)
    else typedScalar(b, dateTime.toString, DataType.Date)
  }

  private def obj(b: Part[T], element: AmfObject, inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = {
      cache.get(element.id) match {
        case Some(value) => b.+=(value)
        case None  if isExternalLink(element) => {
          b.obj(traverse(element, _))
        } // don't add references to the cache, duplicated IDs
        case None => b.obj(traverse(element, _)).foreach(cache.put(element.id, _))
      }
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def isExternalLink(element: AmfObject) = element.isInstanceOf[DomainElement] && element.asInstanceOf[DomainElement].isExternalLink.option().getOrElse(false)

  private def extractToLink(shape: Shape, b: Part[T], inArray: Boolean = false): Unit = {
    if (!ctx.isDeclared(shape) && !ctx.isInReferencedShapes(shape)) {
      ctx + shape
      shape.name.option() match {
        case None =>
          shape.withName(ctx.nextTypeName)
          shape.annotations += InlineElement()
        case Some("schema") | Some("type") =>
          shape.annotations += InlineElement()
        case _ if !shape.annotations.contains(classOf[DeclaredElement]) =>
          shape.annotations += InlineElement() // to catch media type named cases.
        case _ => // ignore
      }
    }
    val linkLabel = shape.name.value()
    val linked = shape match {
      // if it is recursive we force the conversion into a linked shape
      case rec: RecursiveShape =>
        val hash = s"${rec.id}$linkLabel".hashCode
        RecursiveShape()
          .withId(s"${rec.id}/link-$hash")
          .withLinkTarget(rec)
          .withLinkLabel(linkLabel)
      // no recursive we just generate the linked shape
      case _ =>
        shape.link[Shape](linkLabel)
    }

    link(b, linked, inArray)
  }

  private def link(b: Part[T], elementWithLink: DomainElement with Linkable, inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = {
      // before emitting, we remove the link target to avoid loops and set
      // the fresh value for link-id
      val savedLinkTarget = elementWithLink.linkTarget
      elementWithLink.linkTarget.foreach { target =>
        elementWithLink.set(LinkableElementModel.TargetId, target.id)
        elementWithLink.fields.removeField(LinkableElementModel.Target)
      }
      b.obj { o =>
        traverse(elementWithLink, o)
      }
      // we reset the link target after emitting
      savedLinkTarget.foreach { target =>
        elementWithLink.fields.setWithoutId(LinkableElementModel.Target, target)
      }
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def iri(b: Part[T], content: String, inArray: Boolean = false): Unit = {
    // Last update, we assume that the iris are valid and han been encoded. Other option could be use the previous custom lcoal object URLEncoder but not search for %.
    // That can be a problem, because some chars could not being encoded
    def emit(b: Part[T]): Unit = {
      b.obj(_.entry("@id", raw(_, ctx.emitId(content))))
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def safeIri(b: Part[T], content: String, inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = {
      b.obj(_.entry("@id", raw(_, ctx.emitId(content))))
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def typedScalar(b: Part[T], content: String, dataType: String, inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = b.obj { m =>
      m.entry("@value", raw(_, content))
      m.entry("@type", raw(_, ctx.emitIri(dataType)))
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def createIdNode(b: Entry[T], id: String): Unit = b.entry(
      "@id",
      raw(_, ctx.emitId(id))
  )

  private def createSourcesNode(id: String, sources: SourceMap, b: Entry[T]): Unit = {
    if (options.isWithSourceMaps && sources.nonEmpty) {
      if (options.isWithRawSourceMaps) {
        b.entry(
            "smaps",
            _.obj { b =>
              createAnnotationNodes(id, b, sources.annotations)
              createAnnotationNodes(id, b, sources.eternals)
            }
        )
      }
      else {
        b.entry(
            ctx.emitIri(DomainElementModel.Sources.value.iri()),
            _.list {
              _.obj { b =>
                createIdNode(b, id)
                createTypeNode(b, SourceMapModel)
                createAnnotationNodes(id, b, sources.annotations)
                createAnnotationNodes(id, b, sources.eternals)
              }
            }
        )
      }
    }
    else {
      createEternalsAnnotationsNodes(id, options, b, sources)
    }
  }

  private def createEternalsAnnotationsNodes(id: String,
                                             options: RenderOptions,
                                             b: Entry[T],
                                             sources: SourceMap): Unit = {
    if (sources.eternals.nonEmpty)
      if (options.isWithRawSourceMaps) {
        b.entry(
            "smaps",
            _.obj { b =>
              createAnnotationNodes(id, b, sources.eternals)
            }
        )
      }
      else {
        b.entry(
            ctx.emitIri(DomainElementModel.Sources.value.iri()),
            _.list {
              _.obj { b =>
                createIdNode(b, id)
                createTypeNode(b, SourceMapModel)
                createAnnotationNodes(id, b, sources.eternals)
              }
            }
        )
      }
  }

  private def createAnnotationNodes(id: String,
                                    b: Entry[T],
                                    annotations: mutable.ListMap[String, mutable.ListMap[String, String]]): Unit = {
    annotations.foreach({
      case (a, values) =>
        if (ctx.options.isWithRawSourceMaps) {
          b.entry(
              a,
              _.obj { o =>
                values.foreach {
                  case (iri, v) =>
                    o.entry(
                        ctx.emitId(ctx.emitIri(iri)),
                        raw(_, v)
                    )
                }
              }
          )
        }
        else {
          b.entry(
              ctx.emitIri(ValueType(Namespace.SourceMaps, a).iri()),
              _.list(b =>
                values.zipWithIndex.foreach {
                  case (tuple, index) =>
                    createAnnotationValueNode(s"$id/$a/element_$index", b, tuple)
              })
          )
        }
    })
  }

  private def createAnnotationValueNode(id: String, b: Part[T], tuple: (String, String)): Unit =
    tuple match {
      case (iri, v) =>
        b.obj { b =>
          createIdNode(b, id)
          b.entry(ctx.emitIri(SourceMapModel.Element.value.iri()), listWithScalar(_, iri))
          b.entry(ctx.emitIri(SourceMapModel.Value.value.iri()), listWithScalar(_, v))
        }
    }

  private def scalar(b: Part[T], content: String, t: SType): Unit = b.obj(_.entry("@value", Scalar(t, content)))

  private def scalar(b: Part[T], content: String): Unit = scalar(b, content, SType.Str)
  private def scalar(b: Part[T], content: AmfElement, t: SType): Unit =
    scalar(b, content.asInstanceOf[AmfScalar].value.toString, t)

  private def listWithScalar(b: Part[T], content: String, t: SType): Unit = b.list(scalar(_, content, t))
  private def listWithScalar(b: Part[T], content: String): Unit           = listWithScalar(b, content, SType.Str)

  private def listWithScalar(b: Part[T], content: AmfElement, t: SType): Unit = b.list(scalar(_, content, t))
  private def listWithScalar(b: Part[T], content: AmfElement): Unit           = listWithScalar(b, content, SType.Str)

}
