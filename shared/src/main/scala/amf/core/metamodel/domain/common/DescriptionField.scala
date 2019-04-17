package amf.core.metamodel.domain.common

import amf.core.metamodel.Field
import amf.core.metamodel.Type.Str
import amf.core.metamodel.domain.{ModelDoc, ModelVocabularies}
import amf.core.vocabulary.Namespace.Core

/**
  * Description field.
  */
trait DescriptionField {
  val Description = Field(
    Str,
    Core + "description",
    ModelDoc(ModelVocabularies.Core, "description", "Human readable description of an element"))
}

object DescriptionField extends DescriptionField
