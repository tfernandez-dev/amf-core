package amf.plugins.document.graph.emitter

import org.yaml.model.YDocument.PartBuilder
import org.yaml.model._

/**
  * AMF Graph emitter
  */
object DefaultScalarEmitter {

  def scalar(b: PartBuilder, content: String, tag: YType = YType.Str, inArray: Boolean = false): Unit = {
    def emit(b: PartBuilder): Unit = {

      val tg: YType = fixTagIfNeeded(tag, content)

      b.obj(_.entry("@value", raw(_, content, tg)))
    }

    if (inArray) emit(b) else b.list(emit)
  }

  protected def fixTagIfNeeded(tag: YType, content: String): YType = {
    val tg: YType = tag match {
      case YType.Bool =>
        if (content != "true" && content != "false") {
          YType.Str
        }
        else {
          tag
        }
      case YType.Int =>
        try {
          content.toInt
          tag
        } catch {
          case _: NumberFormatException => YType.Str
        }
      case YType.Float =>
        try {
          content.toDouble
          tag
        } catch {
          case _: NumberFormatException => YType.Str
        }
      case _ => tag

    }
    tg
  }

  protected def raw(b: PartBuilder, content: String, tag: YType = YType.Str): Unit =
    b.+=(YNode(YScalar(content), tag))
}
