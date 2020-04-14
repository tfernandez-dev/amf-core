package amf.client.environment

import amf.client.convert.CoreClientConverters._
import amf.client.execution.BaseExecutionEnvironment
import amf.core.unsafe.PlatformSecrets

import scala.concurrent.ExecutionContext
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("client.DefaultEnvironment")
object DefaultEnvironment extends PlatformSecrets {
  @JSExport("apply")
  def apply(): Environment = this.apply(platform.defaultExecutionEnvironment)
  def apply(exec: BaseExecutionEnvironment): Environment = {
    implicit val executionContext: ExecutionContext = exec.executionContext
    val loaders: ClientList[ClientLoader]           = platform.loaders().asClient.asInstanceOf[ClientList[ClientLoader]]
    Environment.empty(exec).withLoaders(loaders)
  }
}
