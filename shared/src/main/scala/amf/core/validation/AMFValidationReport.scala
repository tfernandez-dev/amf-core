package amf.core.validation

import amf.ProfileName

case class AMFValidationReport(conforms: Boolean,
                               model: String,
                               profile: ProfileName,
                               results: Seq[AMFValidationResult]) {

  private val DefaultMax = 30

  def toString(max: Int): String = {
    val str = StringBuilder.newBuilder
    val validations = results.take(max).sorted.groupBy(_.level)

    str.append(s"Model: $model\n")
    str.append(s"Profile: ${profile.profile}\n")
    str.append(s"Conforms? $conforms\n")
    str.append(s"Number of results: ${results.length}\n")

    appendValidations(str, validations, SeverityLevels.VIOLATION)
    appendValidations(str, validations, SeverityLevels.WARNING)
    appendValidations(str, validations, SeverityLevels.INFO)

    str.toString
  }

  private def appendValidations(str: StringBuilder, validations: Map[String, Seq[AMFValidationResult]], level: String): Unit =
    validations.get(level) match {
      case Some(l) =>
        str.append(s"\nLevel: $level\n")
        for { result <- l } {
          str.append(result)
        }
      case None =>
    }

  override def toString: String = toString(DefaultMax)
}

object AMFValidationReport {
  def apply(model: String, profile: ProfileName, results: Seq[AMFValidationResult]) =
    new AMFValidationReport(!results.exists(_.level == SeverityLevels.VIOLATION), model, profile, results)
}
