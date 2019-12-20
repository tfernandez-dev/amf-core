package amf.plugins.features.validation

import amf.ProfileName
import amf.core.validation._
import amf.core.validation.core.ValidationProfile

import scala.collection.mutable

object ParserSideValidationProfiler {

  private val cache : mutable.Map[ProfileName, ValidationProfile] = mutable.Map.empty

  def parserSideValidationsProfile(profile: ProfileName): ValidationProfile = {
    cache.get(profile) match {
      case Some(vp) => vp
      case _ =>
        val vp = computeProfile(profile)
        cache.put(profile, vp)
        vp
    }

  }

  private def computeProfile(profile: ProfileName) = {
    // sorting parser side validation for this profile
    val violationParserSideValidations = Validations.validations
      .filter { v =>
        Validations
          .level(v.id, profile) == SeverityLevels.VIOLATION
      }
      .map(_.name)
    val infoParserSideValidations = Validations.validations
      .filter { v =>
        Validations
          .level(v.id, profile) == SeverityLevels.INFO
      }
      .map(_.name)
    val warningParserSideValidations = Validations.validations
      .filter { v =>
        Validations
          .level(v.id, profile) == SeverityLevels.WARNING
      }
      .map(_.name)
    ValidationProfile(
      name = ProfileName("Parser side AMF Validation"),
      baseProfile = None,
      infoLevel = infoParserSideValidations,
      warningLevel = warningParserSideValidations,
      violationLevel = violationParserSideValidations,
      validations = Validations.validations
    )
  }


}
