package amf.core.execution

import amf.ExecutionContextBuilder

import scala.concurrent.ExecutionContext

case class ExecutionEnvironment(context: ExecutionContext)

object ExecutionEnvironment {
  def apply(): ExecutionEnvironment                          = apply(ExecutionContextBuilder.getGlobalExecutionContext)
  def apply(context: ExecutionContext): ExecutionEnvironment = new ExecutionEnvironment(context)
}
