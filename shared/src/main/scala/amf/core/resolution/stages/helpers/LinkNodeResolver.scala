package amf.core.resolution.stages.helpers

import amf.core.annotations.{DeclaredElement, ResolvedInheritance, ResolvedLinkAnnotation, ResolvedLinkTargetAnnotation}
import amf.core.model.domain.{DomainElement, LinkNode}

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
