package amf.core.registries

import amf.core.metamodel.Obj
import amf.core.model.domain.{AmfObject, AnnotationGraphLoader}
import amf.core.parser.Annotations
import amf.core.plugin.CorePlugin

import scala.collection.mutable

object AMFDomainRegistry {

  def findType(typeString: String): Option[Obj] =
    metadataResolverRegistry.toStream
      .map(_.findType(typeString))
      .filter(_.isDefined)
      .map(_.get)
      .headOption

  def buildType(modelType: Obj): Option[Annotations => AmfObject] =
    metadataResolverRegistry.toStream
      .map(_.buildType(modelType))
      .filter(_.isDefined)
      .map(_.get)
      .headOption

  val annotationsRegistry: mutable.HashMap[String, AnnotationGraphLoader] =
    map(size = 1024, CorePlugin.serializableAnnotations())

  val metadataRegistry: mutable.Map[String, Obj] =
    map(size = 1024, CorePlugin.modelEntities.map(t => defaultIri(t) -> t).toMap)

  private def map[A, B](size: Int, elems: Map[A, B]): mutable.HashMap[A, B] = {
    val r = new mutable.HashMap[A, B] {
      override def initialSize: Int = size
    }
    r ++= elems
  }

  val metadataResolverRegistry: mutable.ListBuffer[AMFDomainEntityResolver] = mutable.ListBuffer.empty

  def registerAnnotation(a: String, agl: AnnotationGraphLoader): Option[AnnotationGraphLoader] =
    annotationsRegistry.put(a, agl)

  def unregisterAnnotation(a: String): Unit = annotationsRegistry.remove(a)

  def registerModelEntity(entity: Obj): Option[Obj] = {
    metadataRegistry.put(defaultIri(entity), entity)
  }

  def unregisterModelEntity(entity: Obj): Unit = {
    metadataRegistry.remove(defaultIri(entity))
  }

  def registerModelEntityResolver(resolver: AMFDomainEntityResolver): Unit = metadataResolverRegistry.append(resolver)

  def unregisterModelEntityResolver(resolver: AMFDomainEntityResolver): Unit = metadataResolverRegistry.-=(resolver)

  def defaultIri(metadata: Obj): String = metadata.`type`.head.iri()
}
