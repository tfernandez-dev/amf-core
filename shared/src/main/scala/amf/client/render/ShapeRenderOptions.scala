package amf.client.render

import amf.client.resolve.ClientErrorHandler
import amf.client.resolve.ClientErrorHandlerConverter.ErrorHandlerConverter
import amf.core.parser.UnhandledErrorHandler

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
  * Json Schema options
  */
@JSExportAll
@JSExportTopLevel("render.ShapeRenderOptions")
class ShapeRenderOptions {

  private var documentation: Boolean = true
  private var eh: ClientErrorHandler = ErrorHandlerConverter.asClient(UnhandledErrorHandler)

  /** Remove documentation items as examples, descriptions, display names, etc. */
  def withoutDocumentation: ShapeRenderOptions = {
    documentation = false
    this
  }

  def withErrorHandler(errorHandler: ClientErrorHandler): ShapeRenderOptions = {
    eh = errorHandler
    this
  }

  def errorHandler: ClientErrorHandler = eh
  def isWithDocumentation: Boolean     = documentation
}

object ShapeRenderOptions {
  def apply(): ShapeRenderOptions = new ShapeRenderOptions()
}
