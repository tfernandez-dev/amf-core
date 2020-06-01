package amf.core.annotations

import amf.core.model.domain.Annotation
import amf.core.parser.Range

case class ReferenceTargets(targets: Map[String, Range]) extends Annotation{
  def + (t:(String,Range)): ReferenceTargets = copy(targets + t)
  def ++ (t:Map[String,Range]): ReferenceTargets = copy(targets ++ t)
}