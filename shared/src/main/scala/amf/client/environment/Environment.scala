package amf.client.environment

import amf.client.convert.CoreClientConverters._
import amf.client.execution.BaseExecutionEnvironment
import amf.client.remote.Content
import amf.client.resource.{ClientResourceLoader, ResourceLoader}
import amf.client.reference.{CachedReference, ClientReferenceResolver, ReferenceResolver}
import amf.core.unsafe.PlatformSecrets
import amf.internal.environment.{Environment => InternalEnvironment}

import scala.concurrent.ExecutionContext
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
case class Environment private (private[amf] val _internal: InternalEnvironment,
                                private val exec: Option[BaseExecutionEnvironment])
    extends PlatformSecrets {

  private implicit val executionContext: ExecutionContext = executionEnvironment.executionContext

  def loaders: ClientList[ResourceLoader]            = _internal.loaders.asClient
  def reference: ClientOption[ReferenceResolver]     = _internal.resolver.asClient
  def executionEnvironment: BaseExecutionEnvironment = exec.getOrElse(platform.defaultExecutionEnvironment)

  @JSExportTopLevel("client.environment.Environment")
  def this() = this(InternalEnvironment.empty(), None)

  def this(execEnv: BaseExecutionEnvironment) = this(InternalEnvironment.empty(), Some(execEnv))

  private def this(internal: InternalEnvironment) = this(internal, None)
  private def this(internal: InternalEnvironment, execEnv: BaseExecutionEnvironment) = this(internal, Some(execEnv))

  def addClientLoader(loader: ClientResourceLoader): Environment = {
    val l = new ResourceLoader {
      override def fetch(resource: String): ClientFuture[Content] = loader.fetch(resource)

      override def accepts(resource: String): Boolean = loader.accepts(resource)
    }
    Environment(_internal.add(ResourceLoaderMatcher.asInternal(l)), exec)
  }

  def withClientResolver(resolver: ClientReferenceResolver): Environment = {
    val r = new ReferenceResolver {
      override def fetch(url: String): ClientFuture[CachedReference] = resolver.fetch(url)
    }
    Environment(_internal.withResolver(ReferenceResolverMatcher.asInternal(r)), exec)
  }

  def add(loader: ClientLoader): Environment = {
    Environment(_internal.add(ResourceLoaderMatcher.asInternal(loader)), exec)
  }

  def withLoaders(loaders: ClientList[ClientLoader]): Environment = {
    val l: ClientList[ResourceLoader] = loaders.asInstanceOf[ClientList[ResourceLoader]]
    Environment(_internal.withLoaders(l.asInternal), exec)
  }

  def withResolver(resolver: ClientReference): Environment = {
    Environment(_internal.withResolver(ReferenceResolverMatcher.asInternal(resolver)), exec)
  }
  /**
    * Defines an upper bound of yaml alias that will be resolved when parsing a DataNode
    */
  def setMaxYamlReferences(value: Long): Environment = {
    Environment(_internal.setMaxYamlReferences(value))
  }
}

object Environment {
  def empty(): Environment                                                          = new Environment()
  def empty(exec: BaseExecutionEnvironment): Environment                            = new Environment(exec)
  def apply(loader: ClientLoader): Environment                                      = empty().add(loader)
  def apply(loader: ClientLoader, exec: BaseExecutionEnvironment): Environment      = empty(exec).add(loader)
  def apply(resolver: ClientReference): Environment                                 = empty().withResolver(resolver)
  def apply(resolver: ClientReference, exec: BaseExecutionEnvironment): Environment = empty(exec).withResolver(resolver)
  def apply(loaders: ClientList[ClientLoader]): Environment                         = empty().withLoaders(loaders)
  def apply(loaders: ClientList[ClientLoader], exec: BaseExecutionEnvironment): Environment =
    empty(exec).withLoaders(loaders)
  def apply(int: InternalEnvironment): Environment                                 = new Environment(int)
  def apply(int: InternalEnvironment, exec: BaseExecutionEnvironment): Environment = new Environment(int, exec)
}
