package amf.internal.environment

import amf.core.unsafe.PlatformSecrets
import amf.internal.reference.ReferenceResolver
import amf.internal.resource.ResourceLoader

case class Environment(loaders: Seq[ResourceLoader], resolver: Option[ReferenceResolver]) {
  def add(loader: ResourceLoader): Environment  = Environment(loader +: loaders, resolver)
  def withLoaders(loaders: Seq[ResourceLoader]) = Environment(loaders, resolver)
  def withResolver(resolver: ReferenceResolver) = Environment(loaders, Some(resolver))
}

object Environment extends PlatformSecrets {
  def apply(): Environment                             = new Environment(platform.loaders(), None)
  def apply(resolver: ReferenceResolver): Environment  = new Environment(Nil, Some(resolver))
  def apply(loaders: Seq[ResourceLoader]): Environment = new Environment(loaders, None)
  def apply(loader: ResourceLoader): Environment       = Environment.apply(Seq(loader))
  def empty(): Environment                             = new Environment(Nil, None)
}
