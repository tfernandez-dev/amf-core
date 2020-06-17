package amf.plugins.document.graph.emitter

import amf.core.annotations.{Declares, References}
import amf.core.metamodel.{Field, Obj}
import amf.core.metamodel.domain.{ExternalSourceElementModel, ShapeModel}
import amf.core.model.domain.{AmfArray, AmfElement, AmfObject, ExternalSourceElement}
import amf.core.parser.{Annotations, FieldEntry}
import amf.plugins.document.graph.MetaModelHelper
import org.yaml.builder.DocBuilder.{Entry, Part}

trait CommonEmitter {

  def extractDeclarationsAndReferencesToContext(declaresEntry: Option[FieldEntry],
                                                referencesEntry: Option[FieldEntry],
                                                annotations: Annotations)(implicit ctx: EmissionContext): ctx.type = {
    val declaredElements: Iterable[AmfElement] =
      declaresEntry.map(_.value.value.asInstanceOf[AmfArray].values).getOrElse(Nil)
    val referencedElements: Iterable[AmfElement] =
      referencesEntry.map(_.value.value.asInstanceOf[AmfArray].values).getOrElse(Nil)

    val declaredIds   = annotations.find(classOf[Declares]).map(_.declares).getOrElse(Nil)
    val referencedIds = annotations.find(classOf[References]).map(_.references).getOrElse(Nil)
    ctx.registerDeclaredAndReferencedFromAnnotations(declaredIds ++ referencedIds)

    ctx ++ declaredElements
    ctx.addReferences(referencedElements)
  }

  def sourceMapIdFor(id: String): String = {
    if (id.endsWith("/")) {
      id + "source-map"
    }
    else if (id.contains("#") || id.startsWith("null")) {
      id + "/source-map"
    }
    else {
      id + "#/source-map"
    }
  }

  def getMetaModelFields(element: AmfObject, obj: Obj): Seq[Field] = {
    val fields = MetaModelHelper.fieldsFrom(obj)
    element match {
      case e: ExternalSourceElement if e.isLinkToSource => fields.filter(f => f != ExternalSourceElementModel.Raw)
      case _                                            => fields
    }
  }

  def getTypesAsIris(obj: Obj): List[String] = obj.`type`.map(_.iri())

  def createTypeNode[T](b: Entry[T], types: List[String])(implicit ctx: EmissionContext): Unit = {
    b.entry(
        "@type",
        _.list { b =>
          types.distinct.foreach(t => raw(b, ctx.emitIri(t)))
        }
    )
  }

  def createTypeNode[T](b: Entry[T], obj: Obj)(implicit ctx: EmissionContext): Unit =
    createTypeNode(b, getTypesAsIris(obj))

  def raw[T](b: Part[T], content: String): Unit = b += content
}
