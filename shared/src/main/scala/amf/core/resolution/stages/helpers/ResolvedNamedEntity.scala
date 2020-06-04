package amf.core.resolution.stages.helpers

import amf.core.model.domain.{Annotation, NamedDomainElement}

import scala.collection.mutable

/**
  * Class to store the mapping of named assigned to the linked entity when resolved.
  * We cannot just overwrite name because that would be overwritten in every single place
  * where the entity has been included
 *
  * @param vals map of names and named entities
  */
case class ResolvedNamedEntity(vals: mutable.HashMap[String, Seq[NamedDomainElement]] = mutable.HashMap())
    extends Annotation
