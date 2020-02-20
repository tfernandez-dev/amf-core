package amf

import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.ExecutionContext

object ExecutionContextBuilder {

  def buildExecutionContext(scheduler: Option[ScheduledExecutorService]): ExecutionContext = scheduler match {
    case Some(s) => buildExecutionContext(s)
    case None    => getGlobalExecutionContext
  }

  def buildExecutionContext(scheduler: ScheduledExecutorService): ExecutionContext =
    ExecutionContext.fromExecutorService(scheduler)

  def getGlobalExecutionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
}
