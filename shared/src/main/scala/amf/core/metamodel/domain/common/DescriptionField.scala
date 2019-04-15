package amf.core.metamodel.domain.common

import amf.core.metamodel.Field
import amf.core.metamodel.Type.Str
import amf.core.metamodel.domain.{ModelDoc, ModelVocabularies}
import amf.core.vocabulary.Namespace.Document

/**
  * Description field.
  */
trait DescriptionField {
  val Description = Field(
    Str,
    Document + "description",
    ModelDoc(ModelVocabularies.AmlDoc, "description", "Human readable description of an element"))
}

object DescriptionField extends DescriptionField
