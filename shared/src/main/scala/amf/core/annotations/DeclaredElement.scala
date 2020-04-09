package amf.core.annotations

import amf.core.metamodel.domain.{DomainElementModel, ModelDoc}
import amf.core.metamodel.{Field, ModelDefaultBuilder, Obj}
import amf.core.model.domain._
import amf.core.vocabulary.ValueType

case class DeclaredElement() extends SerializableAnnotation with PerpetualAnnotation {
  override val name: String = "declared-element"

  override val value: String = ""
}

object DeclaredElement extends AnnotationGraphLoader {
  override def unparse(value: String, objects: Map[String, AmfElement]): Option[Annotation] = Some(DeclaredElement())
}

trait ErrorDeclaration extends DomainElement {
  val namespace: String

  override def withId(value: String): ErrorDeclaration.this.type = super.withId(namespace + value)

  // Temp hack to avoid change the model. If you serialize the model and parsed it again the mark of error will be lost
  override def meta: Obj = new DomainElementModel {
    override def modelInstance: AmfObject = newErrorInstance

    override def fields: List[Field] = originalMeta.fields
    override val `type`: List[ValueType] = originalMeta.`type`
    override val doc:ModelDoc = originalMeta.doc

  }

  protected def newErrorInstance:ErrorDeclaration
  protected def originalMeta:Obj // need to force implementation of the error instance to difference between the normal element and those which are marked as error
}
