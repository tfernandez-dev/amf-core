package amf.core.annotations

import amf.core.model.domain.Annotation
import amf.core.parser.Range

case class ReferenceTargets(targetLocation: String, originRange: Range) extends Annotation