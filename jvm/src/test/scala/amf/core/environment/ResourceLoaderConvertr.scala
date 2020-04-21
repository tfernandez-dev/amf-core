package amf.core.environment

import java.util.concurrent.CompletableFuture

import amf.client.remote.Content
import amf.client.resource.ClientResourceLoader
import org.scalatest.FunSuite
import amf.client.convert.CoreClientConverters._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.compat.java8.FutureConverters._
import scala.concurrent.Future

class ResourceLoaderConvertr extends FunSuite{


  test("Test client resource loader conversion"){
    val cl = new ClientResourceLoader {
      override def fetch(resource: String): CompletableFuture[Content] = {
        CompletableFuture.completedFuture(new Content("#%RAML 1.0 DataType\ntype: string", "jar:/api.raml"))
      }

      override def accepts(resource: String): Boolean = true
    }

    val int = new amf.internal.resource.ResourceLoader {
      override def fetch(resource: String): Future[Content] =
        cl.fetch(resource).toScala

      override def accepts(resource: String): Boolean = cl.accepts(resource)
    }
    ResourceLoaderMatcher.asClient(int)

    succeed
  }
}
