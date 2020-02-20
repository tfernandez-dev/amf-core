package amf.client.resource

import java.net.{HttpURLConnection, SocketTimeoutException}
import java.util.concurrent.CompletableFuture

import amf.client.execution.BaseExecutionEnvironment
import amf.client.remote.Content
import amf.core.lexer.CharArraySequence
import amf.core.remote.FutureConverter._
import amf.core.remote.{NetworkError, SocketTimeout, UnexpectedStatusCode}
import amf.core.unsafe.PlatformSecrets

import scala.concurrent.{ExecutionContext, Future}

class HttpResourceLoader private ()(implicit executionContext: ExecutionContext) extends BaseHttpResourceLoader {

  override def fetch(resource: String): CompletableFuture[Content] = {
    val u          = new java.net.URL(resource)
    val connection = u.openConnection.asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")
    connection.setConnectTimeout(System.getProperty("amf.connection.connect.timeout", "5000").toInt)
    connection.setReadTimeout(System.getProperty("amf.connection.read.timeout", "60000").toInt)

    Future {
      try {
        connection.connect()
        connection.getResponseCode match {
          case 200 =>
            createContent(connection, resource)
          case s =>
            throw UnexpectedStatusCode(resource, s)
        }
      } catch {
        case ex: Exception             => throw NetworkError(ex)
        case e: SocketTimeoutException => throw SocketTimeout(e)
      }
    }.asJava
  }

  private def createContent(connection: HttpURLConnection, url: String): Content = {
    new Content(
      CharArraySequence(connection.getInputStream, connection.getContentLength, None).toString,
      url,
      Option(connection.getHeaderField("Content-Type"))
    )
  }
}

object HttpResourceLoader extends PlatformSecrets {
  def apply(): HttpResourceLoader                               = apply(platform.defaultExecutionEnvironment)
  def apply(exec: BaseExecutionEnvironment): HttpResourceLoader = apply(exec.executionContext)
  def apply(executionContext: ExecutionContext): HttpResourceLoader = new HttpResourceLoader()(executionContext)
}
