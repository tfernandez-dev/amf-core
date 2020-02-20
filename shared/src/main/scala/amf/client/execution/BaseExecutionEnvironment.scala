package amf.client.execution

import amf.core.execution.{ExecutionEnvironment => InternalExecutionEnvironment}

import scala.concurrent.ExecutionContext

abstract class BaseExecutionEnvironment(private[amf] val _internal: InternalExecutionEnvironment) {
  def executionContext: ExecutionContext = _internal.context
}
