package amf.core.resolution.stages

import amf.core.annotations._
import amf.core.metamodel.document.DocumentModel
import amf.core.model.document.{BaseUnit, Document, EncodesModel}
import amf.core.model.domain._
import amf.core.parser.ErrorHandler
import amf.core.resolution.stages.elements.resolution.{ElementResolutionStage, ElementStageTransformer, ReferenceResolution}

import scala.collection.mutable

class ReferenceResolutionStage(keepEditingInfo: Boolean)(
  override implicit val errorHandler: ErrorHandler)
  extends ResolutionStage
  with ElementResolutionStage[DomainElement]{

  var modelResolver: Option[ModelReferenceResolver] = None

  override def resolve[T <: BaseUnit](model: T): T = {
    this.modelResolver = Some(new ModelReferenceResolver(model))
    model.transform(selector, transformation).asInstanceOf[T]
  }

  private def selector(element: DomainElement): Boolean = {
    element match {
      case l: Linkable if l.isLink => true
      case _: LinkNode             => true
      case _                       => false
    }
  }

  private def transformation(element: DomainElement, isCycle: Boolean): Option[DomainElement] = transformer.transform(element)

  def resolveDomainElement[T <: DomainElement](element: T): T = {
    val doc = Document().withId("http://resolutionstage.com/test#")
    if (element.id != null) {
      doc.fields.setWithoutId(DocumentModel.Encodes, element)
    } else {
      doc.withEncodes(element)
    }
    resolve(doc).encodes.asInstanceOf[T]
  }

  def resolveDomainElementSet[T <: DomainElement](elements: Seq[T]): Seq[DomainElement] = {
    val doc = Document().withId("http://resolutionstage.com/test#")

    doc.withDeclares(elements)
    resolve(doc).declares
  }

  protected def customDomainElementTransformation: (DomainElement, Linkable) => DomainElement = (d:DomainElement, _:Linkable) => d

  override def transformer: ElementStageTransformer[DomainElement] = new ReferenceResolution(keepEditingInfo = keepEditingInfo, modelResolver = modelResolver, errorHandler = errorHandler, customDomainElementTransformation = customDomainElementTransformation)
}

class LinkNodeResolutionStage(keepEditingInfo: Boolean, val visited: mutable.Set[String] = mutable.Set())(
  override implicit val errorHandler: ErrorHandler)
  extends ResolutionStage {

  var modelResolver: Option[ModelReferenceResolver] = None

  override def resolve[T <: BaseUnit](model: T): T = {
    this.modelResolver = Some(new ModelReferenceResolver(model))
    model.transform(selector, transformation).asInstanceOf[T]
  }

  private def selector(element: DomainElement): Boolean = {
    if (visited.contains(element.id))
      true
    else {
      visited += element.id
      element match {
        case l: Linkable if l.isLink => true
        case _: LinkNode             => true
        case _                       => false
      }
    }
  }

  private def transformation(element: DomainElement, cycle: Boolean): Option[DomainElement] = {
    element match {
      case ln: LinkNode => LinkNodeResolver.resolveDynamicLink(ln, modelResolver, keepEditingInfo)
      case _            => Some(element)
    }
  }
}

/**
  * Class that holds a resolved dynamic link node, with the original link information
  * @param source the link node that has been resolved
  * @param resolved the piece of domain linked through the resolved link node
  */
class ResolvedLinkNode(val source: LinkNode, val resolved: DomainElement)
// resolved so alias -> value
  extends LinkNode(source.fields, source.annotations) {
  linkedDomainElement = Some(resolved)
}

/**
  * Class to store the mapping of named assigned to the linked entity when resolved.
  * We cannot just overwrite name because that would be overwritten in every single place
  * where the entity has been included
  * @param vals map of names and named entities
  */
case class ResolvedNamedEntity(vals: mutable.HashMap[String, Seq[NamedDomainElement]] = mutable.HashMap())
  extends Annotation

class ModelReferenceResolver(model: BaseUnit) {

  def findFragment(url: String): Option[DomainElement] = {
    model match {
      case encodes: EncodesModel if model.location().exists(_.equals(url)) => Some(encodes.encodes)
      case _ if model.location().exists(_.equals(url))                     => None
      case _ =>
        var remaining                     = model.references.map(new ModelReferenceResolver(_))
        var result: Option[DomainElement] = None
        while (remaining.nonEmpty) {
          remaining.head.findFragment(url) match {
            case res: Some[DomainElement] =>
              result = res
              remaining = Nil
            case _ =>
              remaining = remaining.tail
          }
        }
        result
    }
  }
}

object LinkNodeResolver {

  def resolveDynamicLink(l: LinkNode,
                         modelResolver: Option[ModelReferenceResolver],
                         keepEditingInfo: Boolean): Option[DomainElement] = {
    l match {
      case r: ResolvedLinkNode => Some(r)
      case _ =>
        l.link.option().flatMap(f => modelResolver.flatMap(mr => mr.findFragment(f))) match {
          case Some(elem) =>
            val resolved = new ResolvedLinkNode(l, elem).withId(l.id)
            if (keepEditingInfo) {
              resolved.annotations += ResolvedLinkAnnotation(l.id)
              l.linkedDomainElement.map { element =>
                resolved.annotations += ResolvedLinkTargetAnnotation(element.id)
              }
            }
            resolved.annotations += ResolvedInheritance()
            if (elem.annotations.contains(classOf[DeclaredElement])) resolved.annotations += DeclaredElement()
            Some(resolved)
          case None if l.linkedDomainElement.isDefined =>
            val resolved = new ResolvedLinkNode(l, l.linkedDomainElement.get).withId(l.id)
            if (keepEditingInfo) {
              resolved.annotations += ResolvedLinkAnnotation(l.id)
              l.linkedDomainElement.map { element =>
                resolved.annotations += ResolvedLinkTargetAnnotation(element.id)
              }
            }
            if (l.annotations.contains(classOf[DeclaredElement])) resolved.annotations += DeclaredElement()
            resolved.annotations += ResolvedInheritance()
            Some(resolved)
          case _ =>
            Some(l)
        }
    }
  }
}
