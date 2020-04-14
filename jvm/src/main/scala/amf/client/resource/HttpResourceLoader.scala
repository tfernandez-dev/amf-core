package amf.client.resource

import java.net.{HttpURLConnection, SocketTimeoutException}
import java.util.concurrent.CompletableFuture

import amf.client.execution.BaseExecutionEnvironment
import amf.client.remote.Content
import amf.core.lexer.CharArraySequence
import amf.core.remote.FutureConverter._
import amf.core.remote.{JvmPlatform, NetworkError, SocketTimeout, UnexpectedStatusCode}

import scala.concurrent.{ExecutionContext, Future}

case class HttpResourceLoader(executionContext: ExecutionContext) extends BaseHttpResourceLoader {

  implicit val exec: ExecutionContext = executionContext

  def this() = this(JvmPlatform.instance().defaultExecutionEnvironment.executionContext)
  def this(executionEnvironment: BaseExecutionEnvironment) = this(executionEnvironment.executionContext)

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