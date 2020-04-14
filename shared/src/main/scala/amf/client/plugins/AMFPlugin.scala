package amf.client.plugins

import scala.concurrent.{ExecutionContext, Future}

trait AMFPlugin {

  val ID: String

  def dependencies(): Seq[AMFPlugin]

  def init()(implicit executionContext: ExecutionContext): Future[AMFPlugin]
}
