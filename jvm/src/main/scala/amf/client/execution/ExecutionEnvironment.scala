package amf.client.execution

import java.util.concurrent.ScheduledExecutorService

import amf.ExecutionContextBuilder
import amf.core.execution.{ExecutionEnvironment => InternalExecutionEnvironment}

case class ExecutionEnvironment(override private[amf] val _internal: InternalExecutionEnvironment)
    extends BaseExecutionEnvironment(_internal) {

  def this() = this(InternalExecutionEnvironment())

  def this(scheduler: ScheduledExecutorService) =
    this(InternalExecutionEnvironment(ExecutionContextBuilder.buildExecutionContext(scheduler)))
}

object DefaultExecutionEnvironment {
  def apply(): ExecutionEnvironment = new ExecutionEnvironment()
}
