package amf.core.metamodel.domain.common

import amf.core.metamodel.Field
import amf.core.metamodel.Type.Str
import amf.core.metamodel.domain.{ExternalModelVocabularies, ModelDoc, ModelVocabularies}
import amf.core.vocabulary.Namespace.{Core, Shacl}

/**
  * Name field.
  */
trait NameFieldSchema {
  val Name = Field(Str, Core + "name", ModelDoc(ModelVocabularies.Core, "name", "Name of the shape"))
}

trait NameFieldShacl {
  val Name =
    Field(Str, Shacl + "name", ModelDoc(ExternalModelVocabularies.Shacl, "name", "Additional name for the shape"))
}

object NameFieldSchema extends NameFieldSchema

object NameFieldShacl extends NameFieldShacl
