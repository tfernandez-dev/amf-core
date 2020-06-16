package amf.core.model.document

import amf.core.metamodel.{Field, Obj}
import amf.core.metamodel.document.FragmentModel
import amf.core.metamodel.domain.ModelDoc
import amf.core.model.domain.AmfObject
import amf.core.parser.{Annotations, Fields}
import amf.core.vocabulary.ValueType

case class RecursiveUnit(fields: Fields, annotations: Annotations) extends Fragment{
  override def meta: Obj = new FragmentModel{
    override def fields: List[Field] = FragmentModel.fields

    override val `type`:List[ValueType] = FragmentModel.`type`
    override val doc:ModelDoc = FragmentModel.doc

    override def modelInstance: AmfObject = RecursiveUnit()
  }

  override def componentId: String = "/recursive"
}

object RecursiveUnit {
  def apply(): RecursiveUnit = RecursiveUnit(Fields(), Annotations())
}
