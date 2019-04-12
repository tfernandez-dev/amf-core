package amf.core.model.domain

import amf.core.annotations.{DataNodePropertiesAnnotations, LexicalInformation, ScalarType}
import amf.core.metamodel.Type.{EncodedIri, Str}
import amf.core.metamodel.domain.DataNodeModel._
import amf.core.metamodel.domain.{ScalarNodeModel, _}
import amf.core.metamodel.{DynamicObj, Field, ModelDefaultBuilder, Obj}
import amf.core.model.StrField
import amf.core.model.domain.templates.Variable
import amf.core.parser.{Annotations, Fields}
import amf.core.resolution.VariableReplacer
import amf.core.utils._
import amf.core.vocabulary.Namespace.Data
import amf.core.vocabulary.{Namespace, ValueType}
import org.yaml.model.{YPart, YSequence}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Base class for all dynamic DataNodes
  */
abstract class DataNode(annotations: Annotations) extends DomainElement {

  def name: StrField = fields.field(Name)

  def withName(name: String): this.type = set(Name, name)

  override def adopted(parent: String): this.type = {
    if (Option(id).isEmpty) simpleAdoption(parent) else this
  }

  override def componentId: String = "/" + name.option().getOrElse("data-node").urlComponentEncoded

  /** Replace all raml variables (any name inside double chevrons -> '<<>>') with the provided values. */
  def replaceVariables(values: Set[Variable], keys: Seq[ElementTree])(reportError: String => Unit): DataNode

  def forceAdopted(parent: String): this.type = {
    val adoptedId = parent + "/" + name.option().map(_.urlComponentEncoded).orNull
    val newId = Option(id) match {
      case Some(oldId: String) if oldId.endsWith("/included") => adoptedId + "/included"
      case _                                                  => adoptedId
    }
    withId(newId)
  }

  override val fields: Fields = Fields()

  def cloneNode(): DataNode

  def lexicalPropertiesAnnotation: Option[DataNodePropertiesAnnotations] = None
}

object DataNodeOps {

  /** Adopt entire data node hierarchy. */
  def adoptTree(id: String, node: DataNode): DataNode = {
    node.forceAdopted(id)
    node match {
      case array: ArrayNode =>
        array.members.foreach(adoptTree(array.id, _))
      case obj: ObjectNode =>
        obj.properties.values.foreach(adoptTree(obj.id, _))
      case _ =>
    }
    node
  }
}

/**
  * Data records, with a list of properties
  */
class ObjectNode(override val fields: Fields, val annotations: Annotations) extends DataNode(annotations) {

  val properties
  : mutable.LinkedHashMap[String, DataNode] = mutable.LinkedHashMap() // i need to keep the order of some nodes (like params defined in traits). I could order by position at resolution time, but with this structure i avoid one more traverse.
  val propertyAnnotations: mutable.Map[String, Annotations] =
    annotations.find(classOf[DataNodePropertiesAnnotations]) match {
      case Some(ann) => mutable.HashMap() ++ ann.properties.map(t => t._1 -> Annotations(t._2))
      case _         => mutable.HashMap()
    }

  def addProperty(propertyOrUri: String, objectValue: DataNode, annotations: Annotations = Annotations()): this.type = {
    val property = ensurePlainProperty(propertyOrUri)
    objectValue.adopted(this.id)

    properties += property -> objectValue
    propertyAnnotations.get(property) match {
      case Some(ann) => annotations.foreach(a => if (!ann.contains(a.getClass)) ann += a)
      case None      => propertyAnnotations.update(property, annotations)
    }

    this
  }

  protected def ensurePlainProperty(propertyOrUri: String): String =
    if (propertyOrUri.indexOf(Namespace.Data.base) == 0) {
      propertyOrUri.replace(Namespace.Data.base, "")
    } else {
      propertyOrUri
    }

  override val meta: Obj = new ObjectNodeDynamicModel()

  //  override def valueForField(f: Field): Option[Value] = {
  //    val maybeNode = f.value.ns match {
  //      case Namespace.Data => properties.get(f.value.name)
  //      case _              => None // this or fields.get(f)
  //    }
  //    maybeNode map { Value(_, Annotations()) }
  //  }

  override def replaceVariables(values: Set[Variable], keys: Seq[ElementTree])(
    reportError: String => Unit): DataNode = {
    properties.keys.toSeq.foreach { key =>
      val decodedKey = key.urlComponentDecoded
      val finalKey: String =
        if (decodedKey.endsWith("?")) decodedKey.substring(0, decodedKey.length - 1) else decodedKey
      val maybeTree = keys.find(_.key.equals(finalKey))

      val value = properties(key)
        .replaceVariables(values, maybeTree.map(_.subtrees).getOrElse(Nil))(
          if (decodedKey
            .endsWith("?") && maybeTree.isEmpty) // TODO review this logic
            (_: String) => Unit
          else reportError) // if its an optional node, ignore the violation of the var not implement
      properties.remove(key)
      properties += VariableReplacer.replaceVariablesInKey(decodedKey, values, reportError) -> value
    }

    propertyAnnotations.keys.foreach { key =>
      val value = propertyAnnotations(key)
      propertyAnnotations.remove(key)
      propertyAnnotations += VariableReplacer.replaceVariablesInKey(key.urlComponentDecoded, values, reportError) -> value
    }

    this
  }

  override def cloneNode(): ObjectNode = {
    val cloned = ObjectNode(annotations)

    properties.foreach {
      case (property: String, l: DataNode) =>
        cloned.properties += property          -> l.cloneNode()
        cloned.propertyAnnotations += property -> propertyAnnotations(property)
    }

    cloned
  }

  override def lexicalPropertiesAnnotation: Option[DataNodePropertiesAnnotations] = {
    val stringToInformation = propertyAnnotations.flatMap {
      case (key, ann) => ann.find(classOf[LexicalInformation]).map(l => (key, l))
    }
    if (stringToInformation.nonEmpty) Some(DataNodePropertiesAnnotations(stringToInformation.toMap)) else None
  }

  class ObjectNodeDynamicModel extends DynamicObj with ModelDefaultBuilder {
    override val `type`: List[ValueType] = Data + "Object" :: DataNodeModel.`type`

    override def fields: List[Field] =
      properties.keys.toSeq.sorted
        .map(p => Field(DataNodeModel, Namespace.Data + p, ModelDoc(ModelVocabularies.Data, p)))
        .toList ++ DataNodeModel.fields

    override def modelInstance: AmfObject = ObjectNode()

    override val doc: ModelDoc = ModelDoc(
      ModelVocabularies.Data,
      "Object Node",
      "Node that represents a dynamic object with records data structure"
    )
  }
}

object ObjectNode {

  val builderType: ValueType = Namespace.Data + "Object"

  def apply(): ObjectNode = apply(Annotations())

  def apply(ast: YPart): ObjectNode = apply(Annotations(ast))

  def apply(annotations: Annotations): ObjectNode = new ObjectNode(Fields(), annotations)

}

/**
  * Scalar values with associated data type
  */
class ScalarNode(override val fields: Fields, val annotations: Annotations) extends DataNode(annotations) {

  def withValue(v:String): this.type = set(ScalarNodeModel.Value, v)

  def withDataType(dataType:String): this.type = set(ScalarNodeModel.DataType, dataType)

  def value: StrField = fields.field(ScalarNodeModel.Value)

  def dataType: StrField = fields.field(ScalarNodeModel.DataType)

  override def meta: Obj = ScalarNodeModel

  override def replaceVariables(values: Set[Variable], keys: Seq[ElementTree])(
    reportError: String => Unit): DataNode = {
    VariableReplacer.replaceNodeVariables(this, values, reportError)
  }

  override def cloneNode(): ScalarNode = new ScalarNode(fields, annotations)
}

object ScalarNode {
  def apply(): ScalarNode = apply("", None)

  def apply(annotations: Annotations): ScalarNode = apply("", None, annotations)

  def apply(value: String, dataType: Option[String]): ScalarNode = apply(value, dataType, Annotations())

  def apply(value: String, dataType: Option[String], ast: YPart): ScalarNode =
    apply(value, dataType, Annotations(ast))

  def apply(value: String, dataType: Option[String], annotations: Annotations): ScalarNode = {
    val scalar = new ScalarNode(Fields(), annotations)
    dataType.foreach(annotations += ScalarType(_))
    scalar.set(ScalarNodeModel.DataType, AmfScalar(dataType,Annotations()))
    scalar.set(ScalarNodeModel.Value, AmfScalar(value, annotations))
  }
}

/**
  * Arrays of values
  */
class ArrayNode(override val fields: Fields, val annotations: Annotations) extends DataNode(annotations) {

  val Member: Field = ArrayNodeModel.Member

  var members: ListBuffer[DataNode] = ListBuffer()

  def addMember(member: DataNode): Seq[DataNode] = {
    val array: Seq[DataNode] = fields.field(ArrayNodeModel.Member)
    val newArray             = array :+ member
    set(ArrayNodeModel.Member, AmfArray(newArray))
    newArray
  }

  override def replaceVariables(values: Set[Variable], keys: Seq[ElementTree])(
    reportError: String => Unit): DataNode = {
    members = members.map(_.replaceVariables(values, keys)(reportError))
    this
  }

  override def cloneNode(): this.type = {
    val cloned = ArrayNode(annotations)

    cloned.members = members.map(_.cloneNode())

    cloned.asInstanceOf[this.type]
  }

  def positionFields(): Seq[Field] = members.zipWithIndex.map {
    case (_, i) =>
      Field(EncodedIri, Namespace.Data + s"pos$i", ModelDoc(ModelVocabularies.Data, s"pos$i"))
  }

  override def meta: Obj = new ArrayNodeDynamicModel

  class ArrayNodeDynamicModel extends DynamicObj with ModelDefaultBuilder {
    override def modelInstance: AmfObject = ArrayNode()
    override def fields: List[Field]      = List(Member) ++ positionFields() ++ DataNodeModel.fields
    override val `type`: List[ValueType]  = Data + "Array" :: Namespace.Rdf + "Seq" :: DataNodeModel.`type`
  }
}

object ArrayNode {

  val builderType: ValueType = Namespace.Data + "Array"

  def apply(): ArrayNode = apply(Annotations())

  def apply(ast: YSequence): ArrayNode = apply(Annotations(ast))

  def apply(annotations: Annotations): ArrayNode = new ArrayNode(Fields(), annotations)
}

/**
  * Dynamic node representing a link to another dynamic node
  * @param fields default fields for the dynamic node
  * @param annotations deafult annotations for the dynamic node
  */
class LinkNode(override val fields: Fields, val annotations: Annotations) extends DataNode(annotations) {

  def link: StrField  = fields.field(LinkNodeModel.Value)
  def alias: StrField = fields.field(LinkNodeModel.Alias)

  def withLink(link: String): this.type   = set(LinkNodeModel.Value, link)
  def withAlias(alias: String): this.type = set(LinkNodeModel.Alias, alias)

  var linkedDomainElement: Option[DomainElement] = None

  override def replaceVariables(values: Set[Variable], keys: Seq[ElementTree])(reportError: String => Unit): DataNode =
    this

  override def cloneNode(): LinkNode = {
    val cloned = new LinkNode(fields, annotations)
    cloned.linkedDomainElement = linkedDomainElement
    cloned
  }

  def withLinkedDomainElement(domainElement: DomainElement): LinkNode = {
    linkedDomainElement = Some(domainElement)
    this
  }

  override def meta: Obj = LinkNodeModel

}

object LinkNode {

  val builderType: ValueType = Namespace.Data + "Link"

  def apply(): LinkNode = apply(Annotations())

  def apply(annotations: Annotations): LinkNode = apply("", "", annotations)

  def apply(alias: String, value: String): LinkNode = apply(alias, value, Annotations())

  def apply(alias: String, value: String, annotations: Annotations): LinkNode = {
    val linkNode = new LinkNode(Fields(), annotations)
    linkNode.set(LinkNodeModel.Value, value)
    linkNode.set(LinkNodeModel.Alias, alias)
    linkNode
  }
}

case class ElementTree(key: String, subtrees: Seq[ElementTree])
