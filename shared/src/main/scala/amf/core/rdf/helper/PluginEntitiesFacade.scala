package amf.core.rdf.helper

import amf.core.metamodel.Obj
import amf.core.metamodel.document.{BaseUnitModel, DocumentModel}
import amf.core.model.document.{DeclaresModel, EncodesModel}
import amf.core.model.domain.AmfObject
import amf.core.parser.{Annotations, ParserContext}
import amf.core.rdf.Node
import amf.plugins.features.validation.CoreValidations.UnableToParseNode

class PluginEntitiesFacade(ctx: ParserContext){

  private val sorter                  = new DefaultNodeClassSorter()

  private def isUnitModel(typeModel: Obj): Boolean =
    typeModel.isInstanceOf[DocumentModel] || typeModel.isInstanceOf[EncodesModel] || typeModel
      .isInstanceOf[DeclaresModel] || typeModel.isInstanceOf[BaseUnitModel]

  def retrieveType(id: String, node: Node, findBaseUnit: Boolean = false): Option[Obj] = {
    val types = sorter.sortedClassesOf(node)

    val foundType = types.find { t =>
      val maybeFoundType = findType(t)
      // this is just for self-encoding documents
      maybeFoundType match {
        case Some(typeModel) if !findBaseUnit && !isUnitModel(typeModel) => true
        case Some(typeModel) if findBaseUnit && isUnitModel(typeModel)   => true
        case _                                                           => false
      }
    } orElse {
      // if I cannot find it, I will return the matching one directly, this is used
      // in situations where the references a reified, for example, in the canonical web api spec
      types.find(findType(_).isDefined)
    }

    foundType match {
      case Some(t) => findType(t)
      case None =>
        ctx.eh.violation(UnableToParseNode,
          id,
          s"Error parsing JSON-LD node, unknown @types $types",
          ctx.rootContextDocument)
        None
    }
  }

  private def findType(`type`: String): Option[Obj]            = ctx.plugins.findType(`type`)
  def buildType(`type`: Obj): Annotations => AmfObject = ctx.plugins.buildType(`type`)
}
