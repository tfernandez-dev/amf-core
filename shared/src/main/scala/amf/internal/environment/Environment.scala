package amf.internal.environment

import amf.client.execution.BaseExecutionEnvironment
import amf.core.unsafe.PlatformSecrets
import amf.internal.reference.ReferenceResolver
import amf.internal.resource.ResourceLoader

import scala.concurrent.ExecutionContext

case class Environment(loaders: Seq[ResourceLoader], resolver: Option[ReferenceResolver], maxYamlReferences: Option[Long]) {
  def add(loader: ResourceLoader): Environment  = Environment(loader +: loaders, resolver, maxYamlReferences)
  def withLoaders(loaders: Seq[ResourceLoader]): Environment = Environment(loaders, resolver, maxYamlReferences)
  def withResolver(resolver: ReferenceResolver): Environment = Environment(loaders, Some(resolver), maxYamlReferences)
  def setMaxYamlReferences(value: Long): Environment = Environment(loaders, resolver, Some(value))
}

object Environment extends PlatformSecrets {
  def apply(): Environment = {
    implicit val executionContext: ExecutionContext = platform.defaultExecutionEnvironment.executionContext
    new Environment(platform.loaders(), None, None)
  }

  def apply(exec: ExecutionContext): Environment = {
    implicit val executionContext: ExecutionContext = platform.defaultExecutionEnvironment.executionContext
    new Environment(platform.loaders(), None, None)
  }

  def apply(exec: BaseExecutionEnvironment): Environment = {
    implicit val executionContext: ExecutionContext = exec.executionContext
    new Environment(platform.loaders(exec), None, None)
  }

  def apply(resolver: ReferenceResolver): Environment = new Environment(Nil, Some(resolver), None)

  def apply(loaders: Seq[ResourceLoader]): Environment = new Environment(loaders, None, None)

  def apply(loader: ResourceLoader): Environment = Environment.apply(Seq(loader))

  def empty(): Environment = new Environment(Nil, None, None)
}
