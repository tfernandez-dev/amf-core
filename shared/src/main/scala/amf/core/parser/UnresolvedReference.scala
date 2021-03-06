package amf.core.parser

import amf.core.model.domain.{DomainElement, Linkable}
import amf.plugins.features.validation.CoreValidations.UnresolvedReference

trait UnresolvedReference { this: DomainElement =>
  val reference: String

  // Unresolved references to things that can be linked
  var ctx: Option[ParserContext] = None

  def withContext(c: ParserContext): DomainElement = {
    ctx = Some(c)
    this
  }

  def futureRef(resolve: Linkable => Unit): Unit = ctx match {
    case Some(c) =>
      c.futureDeclarations.futureRef(
        id,
        reference,
        DeclarationPromise(
          resolve,
          () =>
            c.eh.violation(
              UnresolvedReference,
              this,
              None,
              s"Unresolved reference '$reference'"
          )
        )
      )
    case _ => throw new Exception("Cannot create unresolved reference with missing parsing context")
  }

}
