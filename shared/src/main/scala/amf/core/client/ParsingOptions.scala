package amf.core.client
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
  * Parsing options
  */
@JSExportAll
@JSExportTopLevel("parser.ParsingOptions")
class ParsingOptions {
  private var amfJsonLdSerialization          = true
  private var baseUnitUrl: Option[String]     = None
  private var maxYamlReferences: Option[Long] = None

  /**
    * Parse specific AMF JSON-LD serialization
    * @return
    */
  def withoutAmfJsonLdSerialization: ParsingOptions = {
    amfJsonLdSerialization = false
    this
  }

  /**
    * Parse regular JSON-LD serialization
    * @return
    */
  def withAmfJsonLdSerialization: ParsingOptions = {
    amfJsonLdSerialization = true
    this
  }

  def withBaseUnitUrl(baseUnit: String): ParsingOptions = {
    baseUnitUrl = Some(baseUnit)
    this
  }

  def withoutBaseUnitUrl(): ParsingOptions = {
    baseUnitUrl = None
    this
  }

  /**
    * Defines an upper bound of yaml alias that will be resolved when parsing a DataNode
    */
  def setMaxYamlReferences(value: Long): ParsingOptions = {
    maxYamlReferences = Some(value)
    this
  }

  def isAmfJsonLdSerilization: Boolean = amfJsonLdSerialization
  def definedBaseUrl: Option[String]   = baseUnitUrl
  def getMaxYamlReferences: Option[Long]  = maxYamlReferences
}

object ParsingOptions {
  def apply(): ParsingOptions = new ParsingOptions()
}
