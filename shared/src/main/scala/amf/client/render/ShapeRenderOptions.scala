package amf.client.render

import amf.client.resolve.ClientErrorHandler
import amf.client.resolve.ClientErrorHandlerConverter.ErrorHandlerConverter
import amf.core.errorhandling.UnhandledErrorHandler

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
  * Json Schema options
  */
@JSExportAll
@JSExportTopLevel("render.ShapeRenderOptions")
class ShapeRenderOptions {

  private var documentation: Boolean     = true
  private var compactedEmission: Boolean = false
  private var eh: ClientErrorHandler     = ErrorHandlerConverter.asClient(UnhandledErrorHandler)

  /** Remove documentation items as examples, descriptions, display names, etc. */
  def withoutDocumentation: ShapeRenderOptions = {
    documentation = false
    this
  }

  /** Render shape extracting common types to definitions. */
  def withCompactedEmission: ShapeRenderOptions = {
    compactedEmission = true
    this
  }

  def withErrorHandler(errorHandler: ClientErrorHandler): ShapeRenderOptions = {
    eh = errorHandler
    this
  }

  def errorHandler: ClientErrorHandler = eh
  def isWithDocumentation: Boolean     = documentation
  def isWithCompactedEmission: Boolean = compactedEmission
}

object ShapeRenderOptions {
  def apply(): ShapeRenderOptions = new ShapeRenderOptions()
}
