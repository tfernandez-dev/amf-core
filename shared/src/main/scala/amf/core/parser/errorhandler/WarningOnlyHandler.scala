package amf.core.parser.errorhandler
import amf.plugins.features.validation.CoreValidations.SyamlWarning
import org.mulesoft.lexer.SourceLocation
import org.yaml.model.{SyamlException, YError}

case class WarningOnlyHandler(parent:ParserErrorHandler) extends RuntimeWrapperErrorHandler(parent: ParserErrorHandler) {


  override def handle(location: SourceLocation, e: SyamlException): Unit = {
    warning(SyamlWarning, "", e.getMessage, location)
    warningRegister = true
  }

  override def handle[T](error: YError, defaultValue: T): T = {
    warning(SyamlWarning, "", error.error, part(error))
    warningRegister = true
    defaultValue
  }

  private var warningRegister: Boolean = false

  def hasRegister: Boolean = warningRegister
  override  val parserRun: Int = parent.parserRun
}