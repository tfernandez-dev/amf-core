package amf.client.validate

import amf.client.convert.CoreClientConverters._
import amf.client.environment.{DefaultEnvironment, Environment}
import amf.client.model.document.BaseUnit
import amf.core.errorhandling.UnhandledErrorHandler
import amf.core.services.RuntimeValidator
import amf.{AMFStyle, MessageStyle, ProfileName}

import scala.concurrent.ExecutionContext
import scala.scalajs.js.annotation.JSExport

object Validator {

  @JSExport
  def validate(model: BaseUnit,
               profileName: ProfileName,
               messageStyle: MessageStyle = AMFStyle,
               env: Environment = DefaultEnvironment(),
               resolved: Boolean = false): ClientFuture[ValidationReport] = {
    implicit val executionContext: ExecutionContext = env.executionEnvironment.executionContext
    RuntimeValidator(
        model._internal,
        profileName,
        messageStyle,
        env._internal,
        resolved,
        env.executionEnvironment
    ).map(report => report).asClient
  }

  @JSExport
  def loadValidationProfile(url: String, env: Environment = DefaultEnvironment()): ClientFuture[ProfileName] = {
    implicit val executionContext: ExecutionContext = env.executionEnvironment.executionContext
    RuntimeValidator.loadValidationProfile(url, env._internal, UnhandledErrorHandler, env.executionEnvironment).asClient
  }

  @JSExport
  def emitShapesGraph(profileName: ProfileName): String =
    RuntimeValidator.emitShapesGraph(profileName)
}
