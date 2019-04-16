package amf.core.metamodel.document

import amf.core.metamodel.Field
import amf.core.metamodel.Type.Str
import amf.core.metamodel.domain.{DomainElementModel, ModelDoc, ModelVocabularies}
import amf.core.model.domain.AmfObject
import amf.core.vocabulary.Namespace.{Core, Document}
import amf.core.vocabulary.ValueType

/**
  * Fragment meta model.
  *
  * A Module is a parsing Unit that declares DomainElements that can be referenced from the DomainElements in other parsing Units.
  * It main purpose is to expose the declared references so they can be re-used
  */
trait FragmentModel extends BaseUnitModel {

  /**
    * The encodes relationship links a parsing Unit with the DomainElement from a particular domain the unit contains.
    */
  val Encodes = Field(
    DomainElementModel,
    Document + "encodes",
    ModelDoc(
      ModelVocabularies.AmlDoc,
      "encodes",
      "The encodes relationship links a parsing Unit with the DomainElement from a particular domain the unit contains.")
  )

  override def modelInstance: AmfObject =
    throw new Exception("Fragment is abstract instances cannot be created directly")

}

object FragmentModel extends FragmentModel {

  override val `type`: List[ValueType] = List(Document + "Fragment") ++ BaseUnitModel.`type`

  override val fields: List[Field] = Encodes :: BaseUnitModel.fields

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.AmlDoc,
    "Fragment",
    "A Fragment is a parsing Unit that encodes a DomainElement"
  )
}

object PayloadFragmentModel extends FragmentModel {

  val MediaType = Field(
    Str,
    Core + "mediaType",
    ModelDoc(ModelVocabularies.Core, "mediaType", "HTTP Media type associated to the encoded fragment information"))

  override val fields: List[Field] = Encodes :: MediaType :: BaseUnitModel.fields

  override val `type`: List[ValueType] = List(Document + "PayloadFragment") ++ FragmentModel.`type`

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.ApiContract,
    "Payload Fragment",
    "Fragment encoding HTTP payload information"
  )
}
