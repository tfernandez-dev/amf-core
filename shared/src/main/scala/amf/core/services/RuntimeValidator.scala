package amf.core.services

import amf.client.execution.BaseExecutionEnvironment
import amf.core.emitter.RenderOptions
import amf.core.errorhandling.ErrorHandler
import amf.core.metamodel.Field
import amf.core.model.document.BaseUnit
import amf.core.model.domain.DomainElement
import amf.core.parser.Annotations
import amf.core.rdf.RdfModel
import amf.core.services.RuntimeValidator.CustomShaclFunctions
import amf.core.unsafe.PlatformSecrets
import amf.core.validation.core.{ValidationReport, ValidationSpecification}
import amf.core.validation.{AMFValidationReport, AMFValidationResult, EffectiveValidations}
import amf.internal.environment.Environment
import amf.{AMFStyle, MessageStyle, ProfileName}

import scala.concurrent.{ExecutionContext, Future}

trait ValidationsMerger {
  val parserRun: Int

  def merge(result: AMFValidationResult): Boolean
}

object IgnoreValidationsMerger extends ValidationsMerger {
  override val parserRun: Int = -1

  override def merge(result: AMFValidationResult): Boolean = false
}

case class AllValidationsMerger(parserRun: Int) extends ValidationsMerger {
  override def merge(result: AMFValidationResult): Boolean = true
}

/**
  * Validation of AMF models
  */
trait RuntimeValidator extends PlatformSecrets {

  /**
    * Loads a validation profile from a URL
    */
  def loadValidationProfile(validationProfilePath: String,
                            env: Environment = Environment(),
                            errorHandler: ErrorHandler,
                            exec: BaseExecutionEnvironment = platform.defaultExecutionEnvironment): Future[ProfileName]

  def loadValidationProfile(validationProfilePath: String,
                            errorHandler: ErrorHandler,
                            exec: BaseExecutionEnvironment): Future[ProfileName] =
    loadValidationProfile(validationProfilePath, Environment(exec), errorHandler, exec)

  /**
    * Low level validation returning a SHACL validation report
    */
  def shaclValidation(model: BaseUnit,
                      validations: EffectiveValidations,
                      customFunctions: CustomShaclFunctions, // used in customShaclValidator
                      options: ValidationOptions)(implicit executionContext: ExecutionContext): Future[ValidationReport]

  /**
    * Generates a JSON-LD graph with the SHACL shapes for the requested profile name
    *
    * @return JSON-LD graph
    */
  def emitShapesGraph(profileName: ProfileName): String

  /**
    * Returns a native RDF model with the SHACL shapes graph
    */
  def shaclModel(validations: Seq[ValidationSpecification],
                 validationFunctionUrl: String,
                 messgeStyle: MessageStyle = AMFStyle): RdfModel

  /**
    * Main validation function returning an AMF validation report linking validation errors
    * for validations in the profile to domain elements in the model
    */
  def validate(model: BaseUnit,
               profileName: ProfileName,
               messageStyle: MessageStyle,
               env: Environment,
               resolved: Boolean,
               exec: BaseExecutionEnvironment = platform.defaultExecutionEnvironment): Future[AMFValidationReport]

}

object RuntimeValidator {
  var validatorOption: Option[RuntimeValidator] = None

  def register(runtimeValidator: RuntimeValidator): Unit = {
    validatorOption = Some(runtimeValidator)
  }

  private def validator: RuntimeValidator = {
    validatorOption match {
      case Some(runtimeValidator) => runtimeValidator
      case None                   => throw new Exception("No registered runtime validator")
    }
  }

  def loadValidationProfile(validationProfilePath: String,
                            env: Environment = Environment(),
                            errorHandler: ErrorHandler): Future[ProfileName] =
    validator.loadValidationProfile(validationProfilePath, env, errorHandler)

  def loadValidationProfile(validationProfilePath: String,
                            env: Environment,
                            errorHandler: ErrorHandler,
                            executionEnvironment: BaseExecutionEnvironment): Future[ProfileName] =
    validator.loadValidationProfile(validationProfilePath, env, errorHandler, executionEnvironment)

  type PropertyInfo = (Annotations, Field)
  // When no property info is provided violation is thrown in domain element level
  type CustomShaclFunction  = (DomainElement, Option[PropertyInfo] => Unit) => Unit
  type CustomShaclFunctions = Map[String, CustomShaclFunction]

  def shaclValidation(
      model: BaseUnit,
      validations: EffectiveValidations,
      customFunctions: CustomShaclFunctions = Map(), // used for customShaclValidator
      options: ValidationOptions)(implicit executionContext: ExecutionContext): Future[ValidationReport] =
    validator.shaclValidation(model, validations, customFunctions, options)

  def emitShapesGraph(profileName: ProfileName): String =
    validator.emitShapesGraph(profileName)

  def shaclModel(validations: Seq[ValidationSpecification],
                 validationFunctionUrl: String,
                 messageStyle: MessageStyle = AMFStyle): RdfModel =
    validator.shaclModel(validations, validationFunctionUrl, messageStyle)

  def apply(model: BaseUnit,
            profileName: ProfileName,
            messageStyle: MessageStyle = AMFStyle,
            env: Environment = Environment(),
            resolved: Boolean = false): Future[AMFValidationReport] =
    validator.validate(model, profileName, messageStyle, env, resolved)

  def apply(model: BaseUnit,
            profileName: ProfileName,
            messageStyle: MessageStyle,
            env: Environment,
            resolved: Boolean,
            executionEnvironment: BaseExecutionEnvironment): Future[AMFValidationReport] =
    validator.validate(model, profileName, messageStyle, env, resolved, executionEnvironment)

}

class ValidationOptions() {
  val filterFields: Field => Boolean = (_: Field) => false
  var messageStyle: MessageStyle     = AMFStyle
  var level: String                  = "partial" // partial | full

  def toRenderOptions: RenderOptions = RenderOptions().withValidation.withFilterFieldsFunc(filterFields)

  def withMessageStyle(style: MessageStyle): ValidationOptions = {
    messageStyle = style
    this
  }

  def withFullValidation(): ValidationOptions = {
    level = "full"
    this
  }

  def withPartialValidation(): ValidationOptions = {
    level = "partial"
    this
  }

  def isPartialValidation: Boolean = level == "partial"
}

object DefaultValidationOptions extends ValidationOptions {}
