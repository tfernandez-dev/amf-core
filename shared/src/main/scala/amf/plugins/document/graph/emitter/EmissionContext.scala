package amf.plugins.document.graph.emitter

import amf.core.emitter.RenderOptions
import amf.core.model.document.{BaseUnit, Fragment, Module}
import amf.core.model.domain.{AmfElement, DomainElement}
import amf.core.utils.IdCounter
import amf.core.vocabulary.Namespace
import org.yaml.builder.DocBuilder.Entry
import org.yaml.model.YDocument.EntryBuilder

import scala.collection.mutable

class EmissionContext(val prefixes: mutable.Map[String, String],
                      var base: String,
                      val options: RenderOptions,
                      var emittingDeclarations: Boolean = false,
                      var emittingReferences: Boolean = false) {
  var counter: Int = 1

  private val declarations: mutable.LinkedHashSet[AmfElement] = mutable.LinkedHashSet.empty

  private val references: mutable.LinkedHashSet[AmfElement]                   = mutable.LinkedHashSet.empty
  private val normalizedReferenceShapes: mutable.LinkedHashSet[DomainElement] = mutable.LinkedHashSet.empty

  private val validToExtract: mutable.LinkedHashSet[String] = mutable.LinkedHashSet.empty

  private val typeCount: IdCounter = new IdCounter()

  def nextTypeName: String = typeCount.genId("amf_inline_type")

  def emittingDeclarations(d: Boolean): this.type = {
    emittingDeclarations = d
    this
  }

  def emittingReferences(r: Boolean): this.type = {
    emittingReferences = r
    this
  }

  def +(element: AmfElement): this.type = {
    declarations += element
    this
  }

  def ++(elements: Iterable[AmfElement]): this.type = {
    declarations ++= elements
    this
  }

  def addReferences(elements: Iterable[AmfElement]): this.type = {
    references ++= elements
    normalizedReferenceShapes ++= references.collect {
      case fragment: Fragment => Seq(fragment.encodes)
      case lib: Module        => lib.declares
    }.flatten
    this
  }

  /**
    * Used to register shapes that are either declarations or references but are not present in base unit (case of default pipeline)
    * These shapes are registered as they can be extracted to declares and optimize emission of the jsonld.
    */
  def registerDeclaredAndReferencedFromAnnotations(ids: Seq[String]): this.type = {
    validToExtract ++= ids
    this
  }

  def isDeclared(e: AmfElement): Boolean = declarations.contains(e)

  def isInReferencedShapes(e: AmfElement): Boolean = e match {
    case e: DomainElement => normalizedReferenceShapes.contains(e)
    case _                => false
  }

  def canGenerateLink(e: AmfElement): Boolean =
    emittingEncodes && (isDeclared(e) || isInReferencedShapes(e) || canBeExtractedToDeclares(e))

  def canBeExtractedToDeclares(e: AmfElement): Boolean = e match {
    case e: DomainElement => validToExtract.contains(e.id)
    case _                => false
  }

  def emittingEncodes: Boolean = !emittingDeclarations && !emittingReferences

  def declared: Seq[AmfElement] = declarations.toSeq

  def referenced: Seq[AmfElement] = references.toSeq

  def shouldCompact: Boolean = options.isCompactUris

  protected def compactAndCollect(uri: String): String = Namespace.compactAndCollect(uri, prefixes)

  def emitIri(uri: String): String = if (shouldCompact) compactAndCollect(uri) else uri

  def emitId(uri: String): String = {
    if (shouldCompact) {
      if (uri.startsWith(base)) uri.replace(base, "")
      else if (uri.startsWith(baseParent)) uri.replace(s"$baseParent/", "./")
      else uri
    } else uri
  }

  private def baseParent: String = {
    val idx = base.lastIndexOf("/")
    base.substring(0, idx)
  }

  def setupContextBase(location: String): Unit = {
    if (Option(location).isDefined) {
      base = if (location.replace("://", "").contains("/")) {
        val basePre = if (location.contains("#")) {
          location.split("#").head
        } else {
          location
        }
        val parts = basePre.split("/").dropRight(1)
        parts.mkString("/")
      } else {
        location.split("#").head
      }
    } else {
      base = ""
    }
  }

  def emitContext[T](b: Entry[T]): Unit = {
    if (shouldCompact)
      b.entry("@context", _.obj { b =>
        b.entry("@base", base)
        prefixes.foreach {
          case (p, v) =>
            b.entry(p, v)
        }
      })
  }

  def emitContext(b: EntryBuilder): Unit = {
    if (shouldCompact)
      b.entry("@context", _.obj { b =>
        b.entry("@base", base)
        prefixes.foreach {
          case (p, v) =>
            b.entry(p, v)
        }
      })
  }
}

object EmissionContext {
  def apply(unit: BaseUnit, options: RenderOptions) =
    new EmissionContext(mutable.Map(), unit.id, options)
}

class FlattenedEmissionContext(prefixes: mutable.Map[String, String],
                               base: String,
                               options: RenderOptions,
                               emittingDeclarations: Boolean = false)
    extends EmissionContext(prefixes, base, options, emittingDeclarations)

object FlattenedEmissionContext {
  def apply(unit: BaseUnit, options: RenderOptions) =
    new FlattenedEmissionContext(mutable.Map(), unit.id, options)
}
