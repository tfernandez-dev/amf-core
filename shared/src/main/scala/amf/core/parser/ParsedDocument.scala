package amf.core.parser

import org.yaml.model.{YComment, YDocument}

abstract class ParsedDocument

case class SyamlParsedDocument(document: YDocument, comment: Option[YComment] = None) extends ParsedDocument

object SyamlParsedDocument {
  def apply(document: YDocument): SyamlParsedDocument = {
    val comment = document.children collectFirst { case c: YComment => c }
    new SyamlParsedDocument(document, comment)
  }
}
