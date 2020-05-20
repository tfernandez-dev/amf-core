package amf.core.rdf.parsers

import amf.core.annotations.DomainExtensionAnnotation
import amf.core.metamodel.domain.DomainElementModel
import amf.core.metamodel.domain.extensions.DomainExtensionModel
import amf.core.model.domain.DomainElement
import amf.core.model.domain.extensions.{CustomDomainProperty, DomainExtension}
import amf.core.parser.FieldEntry
import amf.core.rdf.graph.NodeFinder
import amf.core.rdf.{Literal, Node, RdfParserCommon, RdfParserContext, Uri}

class CustomPropertiesParser(linkFinder: NodeFinder, sourcesRetriever: SourcesRetriever)(implicit val ctx: RdfParserContext) extends RdfParserCommon{
  def parse(node: Node, instance: DomainElement): Unit = {
    val properties: Seq[String] = node
      .getProperties(DomainElementModel.CustomDomainProperties.value.iri())
      .getOrElse(Nil)
      .filter(_.isInstanceOf[Uri])
      .map(_.asInstanceOf[Uri].value)

    val extensions: Seq[DomainExtension] = properties.flatMap { uri =>
      node
        .getProperties(uri)
        .map(entries => {
          val extension = DomainExtension()
          if (entries.nonEmpty) {
            linkFinder.findLink(entries.head) match {
              case Some(obj) =>
                obj.getProperties(DomainExtensionModel.Name.value.iri()) match {
                  case Some(es) if es.nonEmpty && es.head.isInstanceOf[Literal] =>
                    extension.withName(value(DomainExtensionModel.Name.`type`, es.head.asInstanceOf[Literal].value))
                  case _ => // ignore
                }

                obj.getProperties(DomainExtensionModel.Element.value.iri()) match {
                  case Some(es) if es.nonEmpty && es.head.isInstanceOf[Literal] =>
                    extension.withName(value(DomainExtensionModel.Element.`type`, es.head.asInstanceOf[Literal].value))
                  case _ => // ignore
                }

                val definition = CustomDomainProperty()
                definition.id = uri
                extension.withDefinedBy(definition)

                new DynamicTypeParser(linkFinder, sourcesRetriever).parse(entries.head).foreach { pn =>
                  extension.withId(pn.id)
                  extension.withExtension(pn)
                }

                val sources = sourcesRetriever.retrieve(node)
                extension.annotations ++= annots(sources, extension.id)

              case _ => // ignore
            }
          }
          extension
        })
    }

    if (extensions.nonEmpty) {
      extensions.partition(_.isScalarExtension) match {
        case (scalars, objects) =>
          instance.withCustomDomainProperties(objects)
          applyScalarDomainProperties(instance, scalars)
      }
    }
  }

  private def applyScalarDomainProperties(instance: DomainElement, scalars: Seq[DomainExtension]): Unit = {
    scalars.foreach { e =>
      instance.fields
        .fieldsMeta()
        .find(f => e.element.is(f.value.iri()))
        .foreach(f => {
          instance.fields.entry(f).foreach {
            case FieldEntry(_, value) => value.value.annotations += DomainExtensionAnnotation(e)
          }
        })
    }
  }
}
