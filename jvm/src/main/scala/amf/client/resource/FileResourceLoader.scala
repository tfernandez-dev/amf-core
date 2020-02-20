package amf.client.resource

import java.io.FileNotFoundException
import java.util.concurrent.CompletableFuture

import amf.client.execution.BaseExecutionEnvironment
import amf.client.remote.Content
import amf.core.lexer.FileStream
import amf.core.remote.FileNotFound
import amf.core.remote.FutureConverter._
import amf.core.remote.FileMediaType._
import amf.core.unsafe.PlatformSecrets
import amf.core.utils.AmfStrings

import scala.concurrent.{ExecutionContext, Future}

class FileResourceLoader private ()(implicit executionContext: ExecutionContext) extends BaseFileResourceLoader {

  def fetchFile(resource: String): CompletableFuture[Content] = {
    Future {
      try {
        Content(new FileStream(resource), ensureFileAuthority(resource), extension(resource).flatMap(mimeFromExtension))
      } catch {
        case e: FileNotFoundException =>
          // exception for local file system where we accept spaces [] and other chars in files names
          val decoded = resource.urlDecoded
          try {
            Content(new FileStream(decoded),
                    ensureFileAuthority(resource),
                    extension(resource).flatMap(mimeFromExtension))
          } catch {
            case e: FileNotFoundException => throw FileNotFound(e)
          }
      }
    }.asJava
  }

  def ensureFileAuthority(str: String): String = if (str.startsWith("file:")) str else s"file://$str"
}

object FileResourceLoader extends PlatformSecrets {
  def apply(): FileResourceLoader                               = apply(platform.defaultExecutionEnvironment)
  def apply(exec: BaseExecutionEnvironment): FileResourceLoader = apply(exec.executionContext)
  def apply(executionContext: ExecutionContext): FileResourceLoader = new FileResourceLoader()(executionContext)
}
