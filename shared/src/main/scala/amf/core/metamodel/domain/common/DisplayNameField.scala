package amf.core.metamodel.domain.common

import amf.core.metamodel.Field
import amf.core.metamodel.Type.Str
import amf.core.metamodel.domain.{ModelVocabularies, ModelDoc}
import amf.core.vocabulary.Namespace.Document

/**
  * DisplayName field.
  */
trait DisplayNameField {
  val DisplayName = Field(
    Str,
    Document + "displayName",
    ModelDoc(ModelVocabularies.AmlDoc, "display name", "Human readable name for an entity"))
}

object DisplayNameField extends DisplayNameField
