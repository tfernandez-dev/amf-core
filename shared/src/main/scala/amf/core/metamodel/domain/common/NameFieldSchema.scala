package amf.core.metamodel.domain.common

import amf.core.metamodel.Field
import amf.core.metamodel.Type.Str
import amf.core.metamodel.domain.{ExternalModelVocabularies, ModelDoc, ModelVocabularies}
import amf.core.vocabulary.Namespace.{Document, Shacl}

/**
  * Name field.
  */
trait NameFieldSchema {
  val Name = Field(Str, Document + "name", ModelDoc(ModelVocabularies.AmlDoc, "name", "Name of the shape"))
}

trait NameFieldShacl {
  val Name =
    Field(Str, Shacl + "name", ModelDoc(ExternalModelVocabularies.Shacl, "name", "Additional name for the shape"))
}

object NameFieldSchema extends NameFieldSchema

object NameFieldShacl extends NameFieldShacl
