package amf.client.parse
import amf.core.AMFCompilerRunCount
import amf.core.errorhandling.{AmfResultErrorHandler, ErrorCollector, ErrorHandler, StaticErrorCollector}
import amf.core.parser.errorhandler.AmfParserErrorHandler
import amf.core.validation._



class DefaultParserErrorHandler(override private[amf] val parserRun:Int = AMFCompilerRunCount.nextRun()) extends ErrorCollector with AmfParserErrorHandler {
  override def handlerAmfResult(result: AMFValidationResult): Boolean = synchronized {
    if(super.handlerAmfResult(result)){
      if(parserRun != AMFCompilerRunCount.NONE) StaticErrorCollector.collect(result,parserRun)
      true
    }else false
  }
}

object DefaultParserErrorHandler{
  def apply(): DefaultParserErrorHandler = new DefaultParserErrorHandler(AMFCompilerRunCount.NONE)

  private[amf] def withRun(): DefaultParserErrorHandler = new DefaultParserErrorHandler(AMFCompilerRunCount.nextRun())

  def fromErrorHandler(errorHandler: ErrorHandler): AmfParserErrorHandler = {
    errorHandler match {
      case parser: AmfParserErrorHandler => parser
      case eh:AmfResultErrorHandler =>
        new AmfParserErrorHandler {
          override  val parserRun: Int = AMFCompilerRunCount.NONE
          override def handlerAmfResult(result: AMFValidationResult): Boolean =
            eh.handlerAmfResult(result)
          }
      case eh:ErrorHandler =>
        new AmfParserErrorHandler {
        override  val parserRun: Int = AMFCompilerRunCount.NONE
        override def handlerAmfResult(result:  AMFValidationResult): Boolean = {
          eh.reportConstraint(result.validationId, result.targetNode, result.targetProperty, result.message,result.position, result.level, result.location)
          true
        }
}
    }
  }
}