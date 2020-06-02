package amf.core.parser

import amf.core.annotations.ReferenceTargets
import amf.core.model.document.{BaseUnit, ExternalFragment}
import org.yaml.model.{YNode, YScalar}

case class ParsedReference(unit: BaseUnit, origin: Reference, ast: Option[YNode] = None) {
  def isExternalFragment: Boolean = unit.isInstanceOf[ExternalFragment]


}
