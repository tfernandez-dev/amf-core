package amf.core.plugin

import amf.client.plugins.{AMFDocumentPlugin, AMFDomainPlugin, AMFPlugin}
import amf.core.metamodel.{ModelDefaultBuilder, Obj}
import amf.core.model.domain.AmfObject
import amf.core.parser.Annotations
import amf.core.registries.AMFDomainRegistry.defaultIri
import amf.core.registries.{AMFDomainEntityResolver, AMFDomainRegistry}

import scala.collection.immutable.TreeSet

/** Context for handling plugins registration. */
case class PluginContext(blacklist: Seq[AMFPlugin]) {

  private val blacklistedTypes = blacklist
    .collect({
      case d: AMFDomainPlugin   => d.modelEntities
      case d: AMFDocumentPlugin => d.modelEntities
    })
    .flatten

  private val blacklistedResolvers = blacklist
    .collect({
      case d: AMFDomainPlugin   => d.modelEntitiesResolver
      case d: AMFDocumentPlugin => d.modelEntitiesResolver
    })
    .flatten

  /** Find matching type given type IRI. */
  def findType(`type`: String): Option[Obj] = {
    types
      .get(`type`)
      .orElse(resolvers.iterator.map(_.findType(`type`)).collectFirst {
        case Some(obj) => obj
      })
  }

  /** Return instance builder given type. */
  def buildType(`type`: Obj): Annotations => AmfObject = {
    types.get(defaultIri(`type`)) match {
      case Some(builder: ModelDefaultBuilder) =>
        (annotations: Annotations) =>
          val instance = builder.modelInstance
          instance.annotations ++= annotations
          instance
      case _ =>
        resolvers.iterator
          .map(_.buildType(`type`))
          .collectFirst {
            case Some(builder) => builder
          }
          .getOrElse(throw new Exception(s"Cannot find builder for type ${`type`}"))
    }
  }

  private val types: Map[String, Obj] = {
    val ignored = TreeSet(blacklistedTypes.map(defaultIri): _*)
    AMFDomainRegistry.metadataRegistry.toMap.filterKeys(k => !ignored.contains(k))
  }

  private val resolvers: Seq[AMFDomainEntityResolver] = {
    AMFDomainRegistry.metadataResolverRegistry -- blacklistedResolvers
  }
}

object PluginContext {
  def apply(): PluginContext = new PluginContext(Seq.empty)
}
