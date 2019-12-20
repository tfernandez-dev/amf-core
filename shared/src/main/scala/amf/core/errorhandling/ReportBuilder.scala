package amf.core.errorhandling
import amf.ProfileName
import amf.core.model.document.BaseUnit
import amf.core.validation._
import amf.plugins.features.validation.ParserSideValidationProfiler

class AmfReportBuilder(model:BaseUnit, profileName: ProfileName) {
  def buildReport(results:Seq[AMFValidationResult]): AMFValidationReport = {
    AMFValidationReport(
      conforms = !results.exists(_.level == SeverityLevels.VIOLATION),
      model = model.id,
      profile = profileName,
      results = results
    )
  }
}

class AmfStaticReportBuilder(model:BaseUnit, profileName: ProfileName) extends AmfReportBuilder(model, profileName) with ValidationResultProcessor{

  val validations: EffectiveValidations = EffectiveValidations().someEffective(ParserSideValidationProfiler.parserSideValidationsProfile(profileName))

  def buildFromStatic(): AMFValidationReport = {
    val results = model.parserRun.map(StaticErrorCollector.getRun).getOrElse(Nil).map(processAggregatedResult(_, profileName.messageStyle, validations))
    super.buildReport(results)
  }
}