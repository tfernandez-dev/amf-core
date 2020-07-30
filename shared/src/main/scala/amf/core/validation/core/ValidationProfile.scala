package amf.core.validation.core

import amf.ProfileName
import amf.core.validation.SeverityLevels
import amf.core.validation.core.ValidationProfile.{SeverityLevel, ValidationName}

import scala.collection.immutable.HashMap
import scala.collection.mutable

case class ValidationProfile(name: ProfileName,
                             baseProfile: Option[ProfileName],
                             violationLevel: Seq[String] = Seq.empty,
                             infoLevel: Seq[String] = Seq.empty,
                             warningLevel: Seq[String] = Seq.empty,
                             disabled: Seq[String] = Seq.empty,
                             validations: Seq[ValidationSpecification] = Seq.empty,
                             prefixes: mutable.Map[String, String] = mutable.Map.empty) {

  def validationsWith(severityLevel: SeverityLevel): Seq[ValidationName] = {
    severityLevel match {
      case SeverityLevels.INFO      => infoLevel
      case SeverityLevels.WARNING   => warningLevel
      case SeverityLevels.VIOLATION => violationLevel
    }
  }
}

object ValidationProfile {
  // Circumvent no-typing
  type ValidationName = String
  type ValidationIri  = String
  type SeverityLevel  = String
}
