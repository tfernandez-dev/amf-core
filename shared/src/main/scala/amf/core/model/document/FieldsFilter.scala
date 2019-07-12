package amf.core.model.document
import amf.core.metamodel.document.DocumentModel.References
import amf.core.model.domain.AmfElement
import amf.core.parser.Fields

trait FieldsFilter {
  def filter(fields: Fields): List[AmfElement]
}

object FieldsFilter {

  /** Scope does not include external references. */
  object Local extends FieldsFilter {
    override def filter(fields: Fields): List[AmfElement] =
      fields.fields().collect {
        case e if e.field != References => e.element
      }.toList
  }

  /** Scope includes external references. */
  object All extends FieldsFilter {
    override def filter(fields: Fields): List[AmfElement] =
      fields.fields().map(_.element).toList
  }

}
