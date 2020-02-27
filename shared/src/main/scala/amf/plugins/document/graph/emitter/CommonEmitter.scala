package amf.plugins.document.graph.emitter

import amf.core.annotations.{Declares, References}
import amf.core.model.domain.{AmfArray, AmfElement}
import amf.core.parser.{Annotations, FieldEntry}

trait CommonEmitter {

  def extractDeclarationsAndReferencesToContext(declaresEntry: Option[FieldEntry], referencesEntry: Option[FieldEntry], annotations: Annotations)(implicit ctx: EmissionContext): ctx.type = {
    val declaredElements: Iterable[AmfElement] = declaresEntry.map(_.value.value.asInstanceOf[AmfArray].values).getOrElse(Nil)
    val referencedElements: Iterable[AmfElement] = referencesEntry.map(_.value.value.asInstanceOf[AmfArray].values).getOrElse(Nil)

    val declaredIds = annotations.find(classOf[Declares]).map(_.declares).getOrElse(Nil)
    val referencedIds = annotations.find(classOf[References]).map(_.references).getOrElse(Nil)
    ctx.registerDeclaredAndReferencedFromAnnotations(declaredIds ++ referencedIds)

    ctx ++ declaredElements
    ctx.addReferences(referencedElements)
  }

}
