package amf.core.registries

import amf.core.metamodel.Obj
import amf.core.model.domain.AmfObject
import amf.core.parser.Annotations

trait AMFDomainEntityResolver {

  def findType(typeString: String): Option[Obj]

  def buildType(modelType: Obj): Option[Annotations => AmfObject]
}
