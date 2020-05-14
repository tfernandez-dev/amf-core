package amf.core.resolution.stages.helpers

import amf.core.model.domain.{DomainElement, LinkNode}

/**
  * Class that holds a resolved dynamic link node, with the original link information
 *
  * @param source the link node that has been resolved
  * @param resolved the piece of domain linked through the resolved link node
  */
class ResolvedLinkNode(val source: LinkNode, val resolved: DomainElement)
// resolved so alias -> value
    extends LinkNode(source.fields, source.annotations) {
  linkedDomainElement = Some(resolved)
}
