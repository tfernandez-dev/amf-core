package amf.plugins.document.graph
import amf.core.metamodel.domain.ShapeModel
import amf.core.metamodel.{Field, Obj}

object MetaModelHelper {
  def fieldsFrom(obj: Obj): Seq[Field] = {
    // workaround for lazy values in shape
    val lazyShapeFields = obj match {
      case _: ShapeModel => Seq(ShapeModel.CustomShapePropertyDefinitions, ShapeModel.CustomShapeProperties)
      case _             => Nil
    }
    obj.fields ++ lazyShapeFields
  }
}
