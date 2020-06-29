package amf.plugins.document.graph.parser

trait GraphSerialization

/**
  * JSON-LD serializations
  */
trait JsonLdDocumentForm {
  def name: String
  def extension: String
}

object NoForm extends JsonLdDocumentForm {
  override def extension: String = "jsonld"

  override def name: String = "No form"
}

object FlattenedForm extends JsonLdDocumentForm {
  override def extension: String = "flattened.jsonld"

  override def name: String = "Flattened form"
}

// This is not the actual expanded form, it is the legacy name for the embedded form
object ExpandedForm extends JsonLdDocumentForm {
  override def extension: String = "expanded.jsonld"

  override def name: String = "Expanded form"
}

case class JsonLdSerialization(form: JsonLdDocumentForm) extends GraphSerialization

/**
  * RDF serializations
  */
case class RdfSerialization() extends GraphSerialization
