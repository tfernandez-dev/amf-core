package amf.core.traversal

import amf.core.annotations.{LexicalInformation, SourceAST, SourceNode}
import amf.core.metamodel.document.DocumentModel
import amf.core.model.document.{BaseUnit, DeclaresModel}
import amf.core.model.domain._
import amf.core.parser.{FieldEntry, Value}

import scala.collection.mutable

class TransformationTraversal(val transformation: TransformationData) {

  def traverse(element: AmfObject, traversalData: TraversalData = TraversalData()): AmfObject = {
    if (!traversalData.hasVisited(element)) traverseElement(element, traversalData)
    else element
  }

  private def traverseElement(element: AmfObject, traversalData: TraversalData): AmfObject = {
    // not visited yet
    if (transformation.predicate(element)) { // matches predicate, we transform
      transformation.transformation(element, false) match {
        case Some(transformed: AmfObject) => transformed
        case other                        => other.orNull
      }
    } else {
      // not matches the predicate, we traverse
      // we first process declarations, then the encoding
      traversalData.traversed(element)
      val effectiveFields: Iterable[FieldEntry] = getSortedFieldsOf(element)
      effectiveFields.foreach {
          case fieldEntry @ FieldEntry(_, value) if value.value.isInstanceOf[AmfObject] =>
            traverseObjectEntry(element, traversalData, fieldEntry)
          case fieldEntry @ FieldEntry(_, value) if value.value.isInstanceOf[AmfArray] =>
            traverseArrayEntry(element, traversalData, fieldEntry)

          case _ => // ignore
        }
      element
    }
  }

  private def getSortedFieldsOf(element: AmfObject) = {
    element match {
      case doc: DeclaresModel => doc.fields.fields().toSeq.sorted(new DeclaresModelFieldOrdering)
      case bu: BaseUnit => bu.fields.fields().toSeq.sorted(new BaseUnitFieldOrdering)
      case _ => element.fields.fields()
    }
  }

  private def traverseObjectEntry(element: AmfObject, traversalData: TraversalData, fieldEntry: FieldEntry) = {
    val FieldEntry(field, value) = fieldEntry
    Option(traverse(fieldEntry.obj, traversalData)) match {
      case Some(transformedValue: AmfObject) =>
        element.fields.setWithoutId(field, transformedValue, lexicalAnnotationsOf(value))
        addClosuresToRecursion(element, transformedValue)
      case Some(_) => // ignore
      case None    => element.fields.removeField(field)
    }
  }

  private def lexicalAnnotationsOf(value: Value) = value.annotations.copyFiltering(a =>
    a.isInstanceOf[LexicalInformation] || a.isInstanceOf[SourceAST] || a.isInstanceOf[SourceNode])

  private def traverseArrayEntry(element: AmfObject, traversalData: TraversalData, fieldEntry: FieldEntry) = {
    val FieldEntry(field, value) = fieldEntry
    val newElements = fieldEntry.array.values
      .map {
        case elem: AmfObject =>
          val transformedValue = traverse(elem, traversalData)
          addClosuresToRecursion(element, transformedValue)
          Some(transformedValue)
        case other =>
          Some(other)
      }
      .filter(_.isDefined)
      .map(_.get)
    element.fields.setWithoutId(field, AmfArray(newElements), value.annotations)
  }

  private def addClosuresToRecursion(element: AmfObject, transformedValue: AmfObject): Unit = {
    element match {
      case s: Shape if transformedValue.isInstanceOf[RecursiveShape] =>
        transformedValue
          .asInstanceOf[RecursiveShape]
          .fixpointTarget
          .foreach(t => s.closureShapes += t)
      case _ => // ignore
    }
  }
}

/**
  * Holder for transformation data in transform by condition
  * @param predicate selector
  * @param transformation transformation function
  */
sealed case class TransformationData(predicate: AmfObject => Boolean,
                                     transformation: (AmfObject, Boolean) => Option[AmfObject])

/**
  * Holder for traversal data in transform by condition
  * @param traversed all traversed elements
  */
sealed case class TraversalData(traversed: mutable.Set[String] = mutable.Set()) {
  def hasVisited(element: AmfObject): Boolean = traversed.contains(element.id)
  def traversed(element: AmfObject): Unit     = traversed.add(element.id)
}

class DeclaresModelFieldOrdering extends Ordering[FieldEntry] {
  override def compare(x: FieldEntry, y: FieldEntry): Int = (x.field, y.field) match {
    case (DocumentModel.References, _) => -1
    case (_, DocumentModel.References) => 1
    case (DocumentModel.Declares, _)   => -1
    case (_, DocumentModel.Declares)   => 1
    case (_, _)                        => 0
  }
}

class BaseUnitFieldOrdering extends Ordering[FieldEntry] {
  override def compare(x: FieldEntry, y: FieldEntry): Int = (x.field, y.field) match {
    case (DocumentModel.References, _) => -1
    case (_, DocumentModel.References) => 1
    case (_, _)                        => 0
  }
}
