package amf.client.model.domain

import amf.client.model.StrField
import amf.core.model.domain.{ScalarNode => InternalScalarNode}
import amf.client.convert.CoreClientConverters._

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
@JSExportTopLevel("model.domain.ScalarNode")
case class ScalarNode(override private[amf] val _internal: InternalScalarNode) extends DataNode {

  @JSExportTopLevel("model.domain.ScalarNode")
  def this() = this(InternalScalarNode())

  @JSExportTopLevel("model.domain.ScalarNode")
  def this(value: String, dataType: String) = this(InternalScalarNode(value, Option(dataType)))

  def value: StrField    = _internal.value
  def dataType: StrField = _internal.dataType

  override def toString = s"${name.value()}:$dataType=$value"
}

@JSExportAll
object ScalarNode {
  def build(value: String, dataType: String) = new ScalarNode(value, dataType)
}
