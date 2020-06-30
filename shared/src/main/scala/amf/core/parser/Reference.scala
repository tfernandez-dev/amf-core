package amf.core.parser
import amf.core.benchmark.ExecutionLog
import amf.core.exception.CyclicReferenceException
import amf.core.model.document.{BaseUnit, Module, RecursiveUnit}
import amf.core.services.RuntimeCompiler
import amf.core.unsafe.PlatformSecrets
import amf.core.vocabulary.Namespace
import amf.core.{CompilerContext, parser}
import amf.internal.environment.Environment
import amf.plugins.features.validation.CoreValidations.{ExpectedModule, InvalidInclude}
import org.yaml.model.{YNode, YScalar}

import scala.concurrent.{ExecutionContext, Future}

case class Reference(url: String, refs: Seq[RefContainer]) extends PlatformSecrets {

  def isRemote: Boolean = !url.startsWith("#")

  def +(kind: ReferenceKind, ast: YNode, fragment: Option[String]): Reference = {
    copy(refs = refs :+ RefContainer(kind, ast, fragment))
  }

  def resolve(compilerContext: CompilerContext, nodes: Seq[YNode], allowRecursiveRefs: Boolean)(
      implicit executionContext: ExecutionContext): Future[ReferenceResolutionResult] = {

    // If there is any ReferenceResolver attached to the environment, then first try to get the cached reference if it exists. If not, load and parse as usual.
    try {
      compilerContext.environment.resolver match {
        case Some(resolver) =>
          resolver.fetch(compilerContext.resolvePath(url)) flatMap { cachedReference =>
            Future(ReferenceResolutionResult(None, Some(cachedReference.content)))
          } recoverWith {
            case _ => resolveReference(compilerContext, nodes, allowRecursiveRefs)
          }
        case None => resolveReference(compilerContext, nodes, allowRecursiveRefs)
      }
    } catch {
      case _: Throwable => resolveReference(compilerContext, nodes, allowRecursiveRefs)
    }
  }

  private def resolveReference(compilerContext: CompilerContext, nodes: Seq[YNode], allowRecursiveRefs: Boolean)(
      implicit executionContext: ExecutionContext): Future[ReferenceResolutionResult] = {
    val kinds = refs.map(_.linkType).distinct
    val kind  = if (kinds.size > 1) UnspecifiedReference else kinds.head
    try {
      val context = compilerContext.forReference(url)
      val res: Future[Future[ReferenceResolutionResult]] = RuntimeCompiler.forContext(context, None, None, kind) map {
        eventualUnit =>
          verifyMatchingKind(eventualUnit, kind, kinds, nodes, context.parserContext)
          Future(parser.ReferenceResolutionResult(None, Some(eventualUnit)))
      } recover {
        case e: CyclicReferenceException if allowRecursiveRefs =>
          val fullUrl = e.history.last
          resolveRecursiveUnit(fullUrl, compilerContext).map(u => ReferenceResolutionResult(None, Some(u)))
        case e: Throwable =>
          Future(ReferenceResolutionResult(Some(e), None))
      }
      res flatMap identity
    } catch {
      case e: Throwable => Future(ReferenceResolutionResult(Some(e), None))
    }
  }

  protected def resolveRecursiveUnit(fulllUrl: String, compilerContext: CompilerContext)(
      implicit executionContext: ExecutionContext): Future[RecursiveUnit] = {
    ExecutionLog.log(s"AMFCompiler#parserReferences: Recursive reference $fulllUrl")
    platform.resolve(fulllUrl, compilerContext.environment) map { content =>
      val recUnit = RecursiveUnit().adopted(fulllUrl).withLocation(fulllUrl)
      recUnit.withRaw(content.stream.toString)
      recUnit
    }
  }

  def isInferred: Boolean = refs.exists(_.linkType == InferredLinkReference)

  private def verifyMatchingKind(unit: BaseUnit,
                                 definedKind: ReferenceKind,
                                 allKinds: Seq[ReferenceKind],
                                 nodes: Seq[YNode],
                                 ctx: ParserContext): BaseUnit = {
    unit match {
      case _: Module => // if is a library, kind should be LibraryReference
        if (allKinds.contains(LibraryReference) && allKinds.contains(LinkReference))
          nodes.foreach(
            ctx.eh
              .violation(ExpectedModule, unit.id, "The !include tag must be avoided when referencing a library", _))
        else if (!LibraryReference.eq(definedKind))
          nodes.foreach(ctx.eh.violation(ExpectedModule, unit.id, "Libraries must be applied by using 'uses'", _))
      // ToDo find a better way to skip vocabulary/dialect elements of this validation
      case _ if !unit.meta.`type`.exists(_.iri().contains(Namespace.Meta.base)) =>
        // if is not a library, and is not a vocabulary, kind should not be LibraryReference
        if (LibraryReference.eq(definedKind))
          nodes.foreach(ctx.eh.violation(InvalidInclude, unit.id, "Fragments must be imported by using '!include'", _))
      case _ => // nothing to do
    }
    unit
  }
}
object Reference {
  def apply(url: String, kind: ReferenceKind, node: YNode, fragment: Option[String]): Reference =
    new Reference(url, Seq(RefContainer(kind, node, fragment)))
}
