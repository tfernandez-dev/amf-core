package amf.core.plugin

import amf.client.plugins.{AMFDomainPlugin, AMFPlugin}
import amf.core.annotations._
import amf.core.metamodel.Obj
import amf.core.metamodel.document.{DocumentModel, ExternalFragmentModel, ModuleModel, SourceMapModel}
import amf.core.metamodel.domain.extensions.{CustomDomainPropertyModel, DomainExtensionModel, PropertyShapeModel, ShapeExtensionModel}
import amf.core.metamodel.domain.templates.VariableValueModel
import amf.core.metamodel.domain.{ExternalDomainElementModel, RecursiveShapeModel}
import amf.core.model.domain.AnnotationGraphLoader

import scala.concurrent.{ExecutionContext, Future}

/** Core plugin. No need of registering nor initializing. Used solely for model registry. */
object CorePlugin extends AMFDomainPlugin {

  override def modelEntities: Seq[Obj] = Seq(
    DocumentModel,
    ModuleModel,
    VariableValueModel,
    SourceMapModel,
    RecursiveShapeModel,
    PropertyShapeModel,
    ShapeExtensionModel,
    CustomDomainPropertyModel,
    ExternalFragmentModel,
    ExternalDomainElementModel,
    DomainExtensionModel
  )

  override def serializableAnnotations(): Map[String, AnnotationGraphLoader] = Map (
    "lexical"              -> LexicalInformation,
    "host-lexical"         -> HostLexicalInformation,
    "base-path-lexical"    -> BasePathLexicalInformation,
    "source-vendor"        -> SourceVendor,
    "single-value-array"   -> SingleValueArray,
    "aliases-array"        -> Aliases,
    "synthesized-field"    -> SynthesizedField,
    "default-node"         -> DefaultNode,
    "data-node-properties" -> DataNodePropertiesAnnotations,
    "resolved-link"        -> ResolvedLinkAnnotation,
    "null-security"        -> NullSecurity
  )

  override val ID: String = ""

  override def dependencies(): Seq[AMFPlugin] = Seq.empty

  override def init()(implicit executionContext: ExecutionContext): Future[AMFPlugin] = Future.successful(this)
}
