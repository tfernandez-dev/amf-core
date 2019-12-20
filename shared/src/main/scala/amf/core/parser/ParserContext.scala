package amf.core.parser

import amf.core.model.document.BaseUnit
import amf.core.parser.errorhandler.ParserErrorHandler
import amf.core.validation.core.ValidationSpecification
import org.mulesoft.lexer.SourceLocation
import org.yaml.model.{IllegalTypeHandler, ParseErrorHandler, SyamlException, YError}

import scala.collection.mutable

object EmptyFutureDeclarations {
  def apply(): FutureDeclarations = new FutureDeclarations {}
}

case class ParserContext(rootContextDocument: String = "",
                         refs: Seq[ParsedReference] = Seq.empty,
                         futureDeclarations: FutureDeclarations = EmptyFutureDeclarations(),
                         eh: ParserErrorHandler) extends ParseErrorHandler  with IllegalTypeHandler {

  var globalSpace: mutable.Map[String, Any] = mutable.Map()

  def forLocation(newLocation: String): ParserContext = {
    val copied: ParserContext = this.copy(rootContextDocument = newLocation)
    copied.reportDisambiguation = reportDisambiguation
    copied.globalSpace = globalSpace
    copied
  }

  private val sonsReferences: mutable.Map[String, BaseUnit] = mutable.Map()

  def addSonRef(ref: BaseUnit): this.type = this.synchronized {
    sonsReferences.get(ref.location().getOrElse(ref.id)) match {
      case Some(_) => // ignore
      case _ =>
        sonsReferences.put(ref.location().getOrElse(ref.id), ref)
    }
    this
  }

  private def getSonsParsedReferences: Seq[ParsedReference] =
    sonsReferences.values.map(u => ParsedReference(u, new Reference(u.location().getOrElse(u.id), Nil))).toSeq

  def copyWithSonsReferences(): ParserContext = {
    val context = this.copy(refs = this.refs ++ getSonsParsedReferences)
    context.reportDisambiguation = this.reportDisambiguation
    context.globalSpace = this.globalSpace
    context
  }

  var reportDisambiguation: mutable.Set[String] = mutable.Set()

  val parserRun: Int = eh.parserRun

  override def handle(location: SourceLocation, e: SyamlException): Unit = eh.handle(location, e)

  override def handle[T](error: YError, defaultValue: T): T = eh.handle(error, defaultValue)

  def violation(violationId: ValidationSpecification, node: String, message: String): Unit =
    eh.violation(violationId, node, message, rootContextDocument)

}