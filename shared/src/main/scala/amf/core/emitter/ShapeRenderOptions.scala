package amf.core.emitter

import amf.client.render.{ShapeRenderOptions => ClientShapeRenderOptions}
import amf.client.resolve.ClientErrorHandlerConverter._
import amf.core.errorhandling.{ErrorHandler, UnhandledErrorHandler}

/**
  * JSON Schema options
  */
class ShapeRenderOptions {

  private var documentation: Boolean = true
  private var eh: ErrorHandler       = UnhandledErrorHandler

  /** Remove documentation info as examples, descriptions, display names, etc. */
  def withoutDocumentation: ShapeRenderOptions = {
    documentation = false
    this
  }

  def withErrorHandler(errorHandler: ErrorHandler): ShapeRenderOptions = {
    eh = errorHandler
    this
  }

  def isWithDocumentation: Boolean = documentation
  def errorHandler: ErrorHandler   = eh
}

object ShapeRenderOptions {
  def apply(): ShapeRenderOptions = new ShapeRenderOptions()

  def apply(client: ClientShapeRenderOptions): ShapeRenderOptions = {
    val opts = new ShapeRenderOptions()
    opts.documentation = client.isWithDocumentation
    opts.eh = ErrorHandlerConverter.asInternal(client.errorHandler)
    opts
  }
}
