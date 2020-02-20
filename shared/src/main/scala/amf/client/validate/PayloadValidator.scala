package amf.client.validate

import amf.Core.platform
import amf.client.convert.CoreClientConverters._
import amf.client.execution.BaseExecutionEnvironment
import amf.client.model.document.PayloadFragment
import amf.core.validation.{PayloadValidator => InternalPayloadValidator}

import scala.concurrent.ExecutionContext
import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
class PayloadValidator(private[amf] val _internal: InternalPayloadValidator,
                       private val exec: BaseExecutionEnvironment = platform.defaultExecutionEnvironment) {

  private implicit val executionContext: ExecutionContext = exec.executionContext

  def isValid(mediaType: String, payload: String): ClientFuture[Boolean] =
    _internal.isValid(mediaType, payload).asClient
  def validate(mediaType: String, payload: String): ClientFuture[ValidationReport] =
    _internal.validate(mediaType, payload).asClient
  def validate(payloadFragment: PayloadFragment): ClientFuture[ValidationReport] =
    _internal.validate(payloadFragment).asClient

  def syncValidate(mediaType: String, payload: String): ValidationReport =
    _internal.syncValidate(mediaType, payload)
 }
