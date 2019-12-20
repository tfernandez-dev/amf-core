package amf.core.errorhandling
import amf.core.validation.{AMFValidationReport, AMFValidationResult}

import scala.collection.mutable.ListBuffer

trait ErrorCollector extends AmfResultErrorHandler {
  private val errors: ListBuffer[AMFValidationResult] = ListBuffer()

  override def handlerAmfResult(result: AMFValidationResult): Boolean = synchronized {
    if(!errors.exists(v => v.equals(result))) {
      errors +=  result
      true
    }else false
  }

  def getErrors: List[AMFValidationResult] =  errors.toList
}