package amf.plugins.document.graph.emitter

import amf.core.annotations._
import amf.core.emitter.RenderOptions
import amf.core.metamodel.Type.{Any, Array, ArrayLike, Bool, EncodedIri, Iri, LiteralUri, SortedArray, Str}
import amf.core.metamodel._
import amf.core.metamodel.document.{FragmentModel, ModuleModel, SourceMapModel}
import amf.core.metamodel.domain.extensions.DomainExtensionModel
import amf.core.metamodel.domain.{DomainElementModel, ExternalSourceElementModel, LinkableElementModel, ShapeModel}
import amf.core.model.DataType
import amf.core.model.document.{BaseUnit, EncodesModel, SourceMap}
import amf.core.model.domain.DataNodeOps.adoptTree
import amf.core.model.domain._
import amf.core.model.domain.extensions.DomainExtension
import amf.core.parser.{Annotations, FieldEntry, Value}
import amf.core.vocabulary.{Namespace, ValueType}
import amf.plugins.document.graph.emitter.flattened.utils.{Emission, EmissionQueue, Metadata}
import org.mulesoft.common.time.SimpleDateTime
import org.yaml.builder.DocBuilder
import org.yaml.builder.DocBuilder.{Entry, Part, SType, Scalar}

import scala.language.implicitConversions
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object FlattenedJsonLdEmitter {

  def emit[T](unit: BaseUnit, builder: DocBuilder[T], renderOptions: RenderOptions = RenderOptions()): Boolean = {
    implicit val ctx: EmissionContext = FlattenedEmissionContext(unit, renderOptions)
    new FlattenedJsonLdEmitter[T](builder, renderOptions).root(unit)
    true
  }
}

class FlattenedJsonLdEmitter[T](val builder: DocBuilder[T], val options: RenderOptions)(implicit ctx: EmissionContext)
    extends CommonEmitter
    with MetaModelTypeMapping {

  val pending: EmissionQueue[T] = EmissionQueue()
  var root: Part[T]             = _

  def root(unit: BaseUnit): Unit = {
    builder.obj { ob =>
      // Initialize root object
      ob.entry(
          "@graph",
          _.list { rootBuilder =>
            root = rootBuilder

            /**
              * First queue non declaration elements. We do this because these elements can generate new declarations that we
              * need to know before emiting the Base Unit.
              */
            val declarationsEntry: Option[FieldEntry] = unit.fields.entry(ModuleModel.Declares)
            val referencesEntry: Option[FieldEntry]   = unit.fields.entry(ModuleModel.References)

            extractDeclarationsAndReferencesToContext(declarationsEntry, referencesEntry, unit.annotations)

            unit.fields.removeField(ModuleModel.Declares)
            unit.fields.removeField(ModuleModel.References)

            unit match {
              case u: EncodesModel if isSelfEncoded(u) =>
                /**
                  * If it self encoded we do not queue the encodes node because it will be emitted in the same node as
                  * the base unit
                  */
                queueObjectFieldValues(u, (f, _) => f != FragmentModel.Encodes)
                queueObjectFieldValues(u.encodes) // Still need to queue encodes elements
              case _ =>
                queueObjectFieldValues(unit)
            }

            while (pending.hasPendingEmissions) {
              val emission = pending.nextEmission()
              emission.fn(root)
            }

            /**
              * Emit Base Unit. This will emit declarations also. We don't render the already rendered elements because
              * the queue avoids duplicate ids
              */
            if (isSelfEncoded(unit)) {
              emitSelfEncodedBaseUnitNode(unit)
            }
            else {
              emitBaseUnitNode(unit)
            }

            // Check added declarations
            while (pending.hasPendingEmissions) {
              val emission = pending.nextEmission()
              ctx.emittingDeclarations = emission.isDeclaration
              ctx.emittingReferences = emission.isReference
              emission.fn(root)
            }

            // Now process external links, not declared as part of the unit
            while(pending.hasPendingExternalEmissions) {
              val emission = pending.nextExternalEmission()
              ctx.emittingDeclarations = emission.isDeclaration
              ctx.emittingReferences = emission.isReference
              emission.fn(root)
            }
            // new regular nodes might have been generated, annotations for example
            while (pending.hasPendingEmissions) {
              val emission = pending.nextEmission()
              ctx.emittingDeclarations = emission.isDeclaration
              ctx.emittingReferences = emission.isReference
              emission.fn(root)
            }
          }
      )
      ctx.emitContext(ob)
    }
  }

  private def isSelfEncoded(unit: BaseUnit): Boolean = {
    unit match {
      case e: EncodesModel => Option(e.encodes).exists(_.id == e.id)
      case _               => false
    }
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

  def queueObjectFieldValues(amfObject: AmfObject, filter: (Field, Value) => Boolean = (f, v) => true): Unit = {
    amfObject.fields.foreach {
      case (field, value) if filter(field, value) =>
        field.`type` match {
          case _: Obj =>
            val valueObj = value.value.asInstanceOf[AmfObject]
            pending.tryEnqueue(valueObj)
          case _: ArrayLike =>
            val valueArray = value.value.asInstanceOf[AmfArray]
            valueArray.values.foreach {
              case valueObj: AmfObject => pending.tryEnqueue(valueObj)
              case _                   => //Ignore
            }
          case _ => // Ignore
        }
      case _ => // Ignore
    }
  }

  def emitSelfEncodedBaseUnitNode(unit: BaseUnit): Unit = {
    unit match {
      case u: EncodesModel =>
        root.obj { b =>
          val id         = u.id
          val unitObj    = metaModel(u)
          val encodedObj = metaModel(u.encodes)

          createIdNode(b, id)

          val allTypes = getTypesAsIris(unitObj) ++ getTypesAsIris(encodedObj)
          createTypeNode(b, allTypes)

          emitReferences(b, id, SourceMap(id, unit))
          emitDeclarations(b, id, SourceMap(id, unit))

          val sources = SourceMap(id, unit)

          // Emit both unit and unit.encodes fields to the same node
          emitFields(id, u.encodes, sources, b, getMetaModelFields(u.encodes, encodedObj))

          pending.skip(id) // Skip emitting encodes node (since it is the same as this node)
          emitFields(id, u, sources, b, getMetaModelFields(u, unitObj))

          createCustomExtensions(u, b)

          val sourceMapId: String = sourceMapIdFor(id)
          createSourcesNode(sourceMapId, sources, b)

        }
      case _ => // Exception?
    }
  }

  def emitBaseUnitNode(unit: BaseUnit): Unit = {
    val id = unit.id

    root.obj { b =>
      createIdNode(b, id)
      emitReferences(b, unit.id, SourceMap(unit.id, unit))
      emitDeclarations(b, unit.id, SourceMap(unit.id, unit))

      val sources = SourceMap(id, unit)
      val obj     = metaModel(unit)
      createTypeNode(b, obj)
      traverseMetaModel(id, unit, sources, obj, b)
      createCustomExtensions(unit, b)

      val sourceMapId: String = sourceMapIdFor(id)
      createSourcesNode(sourceMapId, sources, b)

    }

  }

  implicit def object2Emission(amfObject: AmfObject): Emission[T] with Metadata = {
    val id = amfObject.id

    val e = new Emission[T](_ =>
      root.obj { b =>
        emitObject(amfObject, b)
    }) with Metadata
    e.id = Some(id)
    e.isDeclaration = ctx.emittingDeclarations
    e.isReference = ctx.emittingReferences
    e.isExternal = amfObject.isInstanceOf[DomainElement] && amfObject.asInstanceOf[DomainElement].isExternalLink.option().getOrElse(false)
    e
  }

  def emitObject(amfObject: AmfObject, b: Entry[T]): Unit = {
    val id = amfObject.id
    createIdNode(b, id)

    val sources = SourceMap(id, amfObject)

    val obj = metaModel(amfObject)
    createTypeNode(b, obj)
    traverseMetaModel(id, amfObject, sources, obj, b)

    createCustomExtensions(amfObject, b)

    val sourceMapId: String = sourceMapIdFor(id)
    createSourcesNode(sourceMapId, sources, b)
  }

  def traverseMetaModel(id: String, element: AmfObject, sources: SourceMap, obj: Obj, b: Entry[T]): Unit = {
    val modelFields: Seq[Field] = getMetaModelFields(element, obj)

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

    emitFields(id, element, sources, b, modelFields)

  }

  private def emitFields(id: String, element: AmfObject, sources: SourceMap, b: Entry[T], modelFields: Seq[Field]) = {
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
          createIdNode(b, extension.extension.id)
          val e = new Emission((part: Part[T]) => {
            part.obj { rb =>
              rb.entry(
                  ctx.emitIri(DomainExtensionModel.Name.value.iri()),
                  emitScalar(_, extension.name.value())
              )
              field.foreach(
                  f =>
                    rb.entry(
                        ctx.emitIri(DomainExtensionModel.Element.value.iri()),
                        emitScalar(_, f.value.iri())
                  ))
              emitObject(extension.extension, rb)
            }
          }) with Metadata
          e.id = Some(extension.extension.id)
          e.isDeclaration = ctx.emittingDeclarations
          e.isReference = ctx.emittingReferences
          pending.tryEnqueue(e)
        }
    )
  }

  def createSortedArray(b: Part[T],
                        seq: Seq[AmfElement],
                        parent: String,
                        element: Type): Unit = {
    b.obj { b =>
      val id = s"$parent/list"
      createIdNode(b, id)
      val e = new Emission((part: Part[T]) => {
        part.obj {
          rb =>
            createIdNode(rb, id)
            rb.entry("@type", ctx.emitIri((Namespace.Rdfs + "Seq").iri()))
            seq.zipWithIndex.foreach {
              case (e, i) =>
                rb.entry(
                    ctx.emitIri((Namespace.Rdfs + s"_${i + 1}").iri()), {
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
                )
            }
        }
      }) with Metadata
      e.id = Some(id)
      e.isDeclaration = ctx.emittingDeclarations
      e.isReference = ctx.emittingReferences
      pending.tryEnqueue(e)
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
        emitScalar(b, v.value)
        sources(v)
      case Bool =>
        emitScalar(b, v.value, SType.Bool)
        sources(v)
      case Type.Int =>
        emitScalar(b, v.value, SType.Int)
        sources(v)
      case Type.Double =>
        // this will transform the value to double and will not emit @type TODO: ADD YType.Double
        emitScalar(b, v.value, SType.Float)
        sources(v)
      case Type.Float =>
        emitScalar(b, v.value, SType.Float)
        sources(v)
      case Type.DateTime =>
        val dateTime = v.value.asInstanceOf[AmfScalar].value.asInstanceOf[SimpleDateTime]
        typedScalar(b, emitDateFormat(dateTime), DataType.DateTime)
        sources(v)
      case Type.Date =>
        val maybeDateTime = v.value.asInstanceOf[AmfScalar].value match {
          case dt: SimpleDateTime => Some(dt)
          case other              => SimpleDateTime.parse(other.toString).toOption
        }
        maybeDateTime match {
          case Some(dateTime) =>
            if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined) {
              typedScalar(b, emitDateFormat(dateTime), DataType.DateTime)
            }
            else {
              typedScalar(b, f"${dateTime.year}%04d-${dateTime.month}%02d-${dateTime.day}%02d", DataType.Date)

            }
          case _ =>
            emitScalar(b, v.value)
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
                  typedScalar(b, emitDateFormat(dateTime), DataType.DateTime)
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
    if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined) {
      typedScalar(b, emitDateFormat(dateTime), DataType.DateTime, inArray)
    }
    else {
      typedScalar(b, f"${dateTime.year}%04d-${dateTime.month}%02d-${dateTime.day}%02d", DataType.Date)
    }
  }

  private def obj(b: Part[T], obj: AmfObject, inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = {
      b.obj(createIdNode(_, obj.id))
      pending.tryEnqueue(obj)
    }
    emit(b)
  }

  private def extractToLink(shape: Shape, b: Part[T], inArray: Boolean = false): Unit = {
    if (!ctx.isDeclared(shape) && !ctx.isInReferencedShapes(shape)) {
      ctx + shape
      shape.name.option() match {
        case None =>
          shape.withName("inline-type")
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
        createIdNode(o, elementWithLink.id)
        pending.tryEnqueue(elementWithLink)
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
              _.obj {
                b =>
                  // TODO: Maybe this should be emitted in root
                  createIdNode(b, id)
                  val e = new Emission((part: Part[T]) => {
                    part.obj { rb =>
                      createIdNode(rb, id)
                      createTypeNode(rb, SourceMapModel)
                      createAnnotationNodes(id, rb, sources.annotations)
                      createAnnotationNodes(id, rb, sources.eternals)
                    }
                  }) with Metadata
                  e.id = Some(id)
                  e.isDeclaration = ctx.emittingDeclarations
                  e.isReference = ctx.emittingReferences
                  pending.tryEnqueue(e)
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
                val e = new Emission((part: Part[T]) => {
                  part.obj { rb =>
                    createIdNode(rb, id)
                    createTypeNode(rb, SourceMapModel)
                    createAnnotationNodes(id, rb, sources.eternals)
                  }
                }) with Metadata

                e.id = Some(id)
                e.isDeclaration = ctx.emittingDeclarations
                e.isReference = ctx.emittingReferences
                pending.tryEnqueue(e)
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
                values.zipWithIndex.foreach { // sortear
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
        }
        val e = new Emission((part: Part[T]) => {
          part.obj { b =>
            createIdNode(b, id)
            b.entry(ctx.emitIri(SourceMapModel.Element.value.iri()), emitScalar(_, iri))
            b.entry(ctx.emitIri(SourceMapModel.Value.value.iri()), emitScalar(_, v))
          }
        }) with Metadata
        e.id = Some(id)
        e.isDeclaration = ctx.emittingDeclarations
        e.isReference = ctx.emittingReferences
        pending.tryEnqueue(e)
    }

  private def emitDateFormat(dateTime: SimpleDateTime) = dateTime.toString

  private def scalar(b: Part[T], content: String, t: SType): Unit = b += Scalar(t, content)

  private def scalar(b: Part[T], content: String): Unit = scalar(b, content, SType.Str)

  private def scalar(b: Part[T], content: AmfElement, t: SType): Unit =
    scalar(b, content.asInstanceOf[AmfScalar].value.toString, t)

  private def emitScalar(b: Part[T], content: String, t: SType): Unit = scalar(b, content, t)
  private def emitScalar(b: Part[T], content: String): Unit           = emitScalar(b, content, SType.Str)

  private def emitScalar(b: Part[T], content: AmfElement, t: SType): Unit = scalar(b, content, t)
  private def emitScalar(b: Part[T], content: AmfElement): Unit           = emitScalar(b, content, SType.Str)

}
