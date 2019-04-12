package amf.core.model.domain

import amf.core.metamodel.Field
import amf.core.parser.{FieldEntry, Value}
import amf.core.vocabulary.Namespace

case class Graph(e: DomainElement) {

  def removeField(uri: String): this.type = {
    e.fields.remove(uri)
    this
  }

  def types(): Seq[String] = e.meta.`type`.map(_.iri()).distinct

  def properties(): Seq[String] = e.fields.fields().map(_.field.value.iri()).toSeq

  def scalarByField(field: Field): Seq[Any] = scalarByProperty(field.toString)

  def scalarByProperty(propertyId: String): Seq[Any] = {
    e.fields.fields().find { f: FieldEntry =>
      f.field.value.iri() == Namespace.uri(propertyId).iri()
    } match {
      case Some(fieldEntry) =>
        fieldEntry.element match {
          case scalar: AmfScalar                    => List(scalar.value)
          case arr: AmfArray if arr.values.nonEmpty => arr.values.toList
          case _                                    => List()
        }
      case None => List()
    }
  }

  def getObjectByPropertyId(propertyId: String): Seq[DomainElement] = {
    e.fields.fields().find { f: FieldEntry =>
      f.field.value.iri() == Namespace.uri(propertyId).iri()
    } match {
      case Some(fieldEntry) =>
        fieldEntry.element match {
          case entity: DomainElement => List(entity)
          case arr: AmfArray if arr.values.nonEmpty && arr.values.head.isInstanceOf[DomainElement] =>
            arr.values.map(_.asInstanceOf[DomainElement]).toList
          case _ => List()
        }
      case None => List()
    }
  }

  def containsField(f:Field): Boolean = properties().contains(f.toString)

  def patchField(patchField: Field, patchValue: Value): Unit = {
    e.set(patchField, patchValue.value, e.fields.getValue(patchField).annotations)
  }

  def patchObj(patchField: Field, mergedNode: AmfObject): Unit = {
    e.set(patchField, mergedNode, e.fields.getValue(patchField).annotations)
  }

  def patchSeqField(patchField: Field, objs: Seq[AmfElement]): Unit = {
    e.set(patchField, AmfArray(objs), e.fields.getValue(patchField).annotations)
  }
}
