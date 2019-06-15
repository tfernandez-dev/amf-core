package amf.core.annotations

import amf.core.model.domain._

/**
  * Keeps tack of the original link target information after resolution
  * @param linkTargetId id of the linked (target) domain element
  */
case class ResolvedLinkTargetAnnotation(linkTargetId: String)
    extends SerializableAnnotation
    with PerpetualAnnotation
    with UriAnnotation {

  override val name: String                              = "resolved-link-target"
  override val value: String                             = linkTargetId
  override val uris: Seq[String]                         = Seq(linkTargetId)
  override def shorten(fn: String => String): Annotation = ResolvedLinkTargetAnnotation(fn(linkTargetId))
}

object ResolvedLinkTargetAnnotation extends AnnotationGraphLoader {
  override def unparse(value: String, objects: Map[String, AmfElement]): Option[Annotation] =
    Some(ResolvedLinkTargetAnnotation(value))
}
