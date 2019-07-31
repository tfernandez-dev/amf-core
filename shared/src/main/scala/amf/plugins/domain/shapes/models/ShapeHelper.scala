package amf.plugins.domain.shapes.models

import amf.core.metamodel.domain.ShapeModel
import amf.core.model.domain.{AmfScalar, Shape}
import amf.core.parser.Annotations
import org.yaml.model.YMapEntry
import org.yaml.render.YamlRender

trait ShapeHelper { this: Shape =>

  def setDefaultStrValue(entry: YMapEntry): Unit = {
    val str = entry.value.asScalar match {
      case Some(s) => s.text
      case _       => YamlRender.render(entry.value)
    }
    this.set(ShapeModel.DefaultValueString, AmfScalar(str), Annotations(entry))
  }

}
