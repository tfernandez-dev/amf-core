package amf.core.parser.errorhandler
import amf.core.AMFCompilerRunCount
import amf.core.validation.AMFValidationResult

object UnhandledParserErrorHandler extends UnhandledAmfParserErrorHandler{}

trait UnhandledAmfParserErrorHandler extends AmfParserErrorHandler {
  override  val parserRun: Int = AMFCompilerRunCount.NONE
  override def handlerAmfResult(result: AMFValidationResult): Boolean = {
    throw new Exception(
      s"  Message: ${result.message}\n  Target: ${result.targetNode}\nProperty: ${result.targetProperty.getOrElse("")}\n  Position: ${result.position}\n at location: ${result.location}")

  }
}