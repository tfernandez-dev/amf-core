package amf.core.rdf.parsers

import amf.core.metamodel.domain.{ArrayNodeModel, DomainElementModel, LinkNodeModel, ScalarNodeModel}
import amf.core.model.domain
import amf.core.model.domain.{AmfObject, AmfScalar, Annotation, ArrayNode, DataNode, DomainElement, LinkNode, ObjectNode, ScalarNode}
import amf.core.parser.Annotations
import amf.core.rdf.{DefaultNodeClassSorter, Literal, Node, PropertyObject, RdfParserCommon, RdfParserContext, Uri}
import amf.core.rdf.graph.NodeFinder
import amf.core.vocabulary.Namespace

import scala.collection.mutable

class DynamicTypeParser(linkFinder: NodeFinder, sourcesRetriever: SourcesRetriever, sorter: DefaultNodeClassSorter = new DefaultNodeClassSorter())(implicit val ctx: RdfParserContext) extends RdfParserCommon{

  def parse(property: PropertyObject): Option[DataNode] = {
    linkFinder.findLink(property).map { node =>
      val sources = sourcesRetriever.retrieve(node)
      val builder = retrieveDynamicType(node).get

      builder(annots(sources, property.value)) match {
        case obj: ObjectNode =>
          obj.withId(node.subject)
          node.getKeys().foreach { uri =>
            if (uri != "@type" && uri != "@id" && uri != DomainElementModel.Sources.value.iri() &&
              uri != (Namespace.Core + "name").iri()) { // we do this to prevent parsing name of annotations

              val dataNode = node.getProperties(uri).get.head match {
                case literal @ Literal(_, _) => new DynamicLiteralParser().parse(literal)
                case entry if isRDFArray(entry) => new DynamicArrayParser(linkFinder, sourcesRetriever).parse(entry)
                case nestedNode @ Uri(_) => parse(nestedNode).getOrElse(ObjectNode())
                case _ => ObjectNode()
              }
              obj.addProperty(uri, dataNode)
            }
          }
          obj

        case scalar: ScalarNode =>
          scalar.withId(node.subject)
          node.getKeys().foreach { k =>
            val entries = node.getProperties(k).get
            if (k == ScalarNodeModel.Value.value.iri() && entries.head.isInstanceOf[Literal]) {
              val parsedScalar = new DynamicLiteralParser().parse(entries.head.asInstanceOf[Literal])

              parsedScalar.value.option().foreach { v =>
                scalar.set(ScalarNodeModel.Value, AmfScalar(v, parsedScalar.value.annotations()))
              }
            }
          }
          scalar

        case link: LinkNode =>
          link.withId(node.subject)
          node.getKeys().foreach { k =>
            val entries = node.getProperties(k).get
            if (k == LinkNodeModel.Alias.value.iri() && entries.head.isInstanceOf[Literal]) {
              val parsedScalar = new DynamicLiteralParser().parse(entries.head.asInstanceOf[Literal])
              parsedScalar.value.option().foreach(link.withAlias)
            } else if (k == LinkNodeModel.Value.value.iri() && entries.head.isInstanceOf[Literal]) {
              val parsedScalar = new DynamicLiteralParser().parse(entries.head.asInstanceOf[Literal])
              parsedScalar.value.option().foreach(link.withLink)
            }
          }
          ctx.referencesMap.get(link.alias.value()) match {
            case Some(target) => link.withLinkedDomainElement(target)
            case _ =>
              val unresolved: Seq[DomainElement] = ctx.unresolvedReferences.getOrElse(link.alias.value(), Nil)
              ctx.unresolvedReferences += (link.alias.value() -> (unresolved ++ Seq(link)))
          }
          link

        case array: ArrayNode =>
          array.withId(node.subject)
          node.getKeys().foreach { k =>
            if (k == ArrayNodeModel.Member.value.iri()) {
              array.withMembers(node.getProperties(k).getOrElse(Nil).flatMap(parse))
            }
          }
          array

        case other =>
          throw new Exception(s"Cannot parse object data node from non object JSON structure $other")
      }
    }
  }

  def isRDFArray(entry: PropertyObject): Boolean = {
    entry match {
      case id @ Uri(_) =>
        linkFinder.findLink(id) match {
          case Some(node) =>
            node.getProperties((Namespace.Rdf + "first").iri()).isDefined ||
              node.getProperties((Namespace.Rdf + "rest").iri()).isDefined
          case _ => false
        }
      case _ => false
    }
  }


  def retrieveDynamicType(node: Node): Option[Annotations => AmfObject] = {
    sorter.sortedClassesOf(node).find({ t =>
      dynamicBuilders.contains(t)
    }) match {
      case Some(t) => Some(dynamicBuilders(t))
      case _       => None
    }
  }

  private val dynamicBuilders: mutable.Map[String, Annotations => AmfObject] = mutable.Map(
    LinkNode.builderType.iri()        -> domain.LinkNode.apply,
    ArrayNode.builderType.iri()       -> domain.ArrayNode.apply,
    ScalarNodeModel.`type`.head.iri() -> domain.ScalarNode.apply,
    ObjectNode.builderType.iri()      -> domain.ObjectNode.apply
  )
}
