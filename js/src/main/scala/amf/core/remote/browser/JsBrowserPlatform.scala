package amf.core.remote.browser

import amf.client.execution.BaseExecutionEnvironment
import amf.internal.resource.{ResourceLoader, ResourceLoaderAdapter}
import amf.core.remote._
import org.mulesoft.common.io.FileSystem

import scala.concurrent.ExecutionContext
import scala.scalajs.js.annotation.JSExportAll

/**
  *
  */
class JsBrowserPlatform extends JsPlatform {

  /** Underlying file system for platform. */
  override val fs: FileSystem = UnsupportedFileSystem

  /** Platform out of the box [ResourceLoader]s */
  override def loaders(exec: BaseExecutionEnvironment = defaultExecutionEnvironment): Seq[ResourceLoader] = {
    implicit val executionContext: ExecutionContext = exec.executionContext
    loaders()
  }

  /** Platform out of the box [ResourceLoader]s */
  override def loaders()(implicit executionContext: ExecutionContext): Seq[ResourceLoader] =
    Seq(ResourceLoaderAdapter(JsBrowserHttpResourceLoader()))


  /** Return temporary directory. */
  override def tmpdir(): String = {
    // Accept in Node only
    throw new Exception(s"Unsupported tmpdir operation")
  }

  override def operativeSystem(): String = "web"

  override def resolvePath(path: String): String = path

}

@JSExportAll
object JsBrowserPlatform {
  private var singleton: Option[JsBrowserPlatform] = None

  def instance(): JsBrowserPlatform = singleton match {
    case Some(p) => p
    case None =>
      singleton = Some(new JsBrowserPlatform())
      singleton.get
  }
}
