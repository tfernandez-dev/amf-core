package amf.core.rdf.parsers

import amf.core.model.domain.ScalarNode
import amf.core.rdf.Literal

class DynamicLiteralParser {
  def parse(l: Literal): ScalarNode = {
    val result = ScalarNode()
    l.literalType.foreach(t => result.withDataType(t))
    result.withValue(l.value)
    result
  }
}
