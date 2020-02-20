package amf.client.validate

import amf.client.convert.CoreClientConverters._
import amf.client.environment.{DefaultEnvironment, Environment}
import amf.internal.environment.{Environment => InternalEnvironment}
import amf.client.execution.BaseExecutionEnvironment
import amf.client.model.document.BaseUnit
import amf.core.errorhandling.UnhandledErrorHandler
import amf.core.services.RuntimeValidator
import amf.core.unsafe.PlatformSecrets
import amf.{AMFStyle, MessageStyle, ProfileName}

import scala.concurrent.ExecutionContext
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

object Validator extends PlatformSecrets {

  @JSExport
  def validate(model: BaseUnit,
               profileName: ProfileName,
               messageStyle: MessageStyle = AMFStyle,
               env: Environment = DefaultEnvironment(),
               resolved: Boolean = false): ClientFuture[ValidationReport] = {
    implicit val executionContext: ExecutionContext = platform.defaultExecutionEnvironment.executionContext
    RuntimeValidator(
        model._internal,
        profileName,
        messageStyle,
        env._internal,
        resolved
    ).map(report => report).asClient
  }

  def validateWithExecutionEnvironment(model: BaseUnit,
                                       profileName: ProfileName,
                                       executionEnv: BaseExecutionEnvironment,
                                       messageStyle: MessageStyle = AMFStyle,
                                       env: ClientOption[Environment],
                                       resolved: Boolean = false): ClientFuture[ValidationReport] = {
    implicit val executionContext: ExecutionContext = executionEnv.executionContext
    val environment: InternalEnvironment            = env.toScala.getOrElse(DefaultEnvironment(executionEnv))._internal
    RuntimeValidator(
        model._internal,
        profileName,
        messageStyle,
        environment,
        resolved,
        executionEnv
    ).map(report => report).asClient
  }

  @JSExport
  def loadValidationProfile(url: String, env: Environment = DefaultEnvironment()): ClientFuture[ProfileName] = {
    implicit val executionContext: ExecutionContext = platform.defaultExecutionEnvironment.executionContext
    RuntimeValidator.loadValidationProfile(url, env._internal, UnhandledErrorHandler).asClient
  }

  def loadValidationProfileWithExecutionEnvironment(
      url: String,
      env: ClientOption[Environment],
      executionEnv: BaseExecutionEnvironment): ClientFuture[ProfileName] = {
    implicit val executionContext: ExecutionContext = executionEnv.executionContext
    val environment: InternalEnvironment            = env.toScala.getOrElse(DefaultEnvironment(executionEnv))._internal
    RuntimeValidator
      .loadValidationProfile(url, environment, UnhandledErrorHandler, executionEnv)
      .asClient
  }

  @JSExport
  def emitShapesGraph(profileName: ProfileName): String =
    RuntimeValidator.emitShapesGraph(profileName)
}
