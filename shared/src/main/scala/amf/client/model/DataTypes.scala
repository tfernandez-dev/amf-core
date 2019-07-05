package amf.client.model

import amf.core.vocabulary.Namespace
import amf.core.vocabulary.Namespace.Xsd
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
@JSExportTopLevel("client.model.DataTypes")
object DataTypes {
  val String: String       = Xsd.base + "string"
  val Integer: String      = Xsd.base + "integer"
  val Number: String       = Namespace.Shapes.base + "number"
  val Long: String         = Xsd.base + "long"
  val Double: String       = Xsd.base + "double"
  val Float: String        = Xsd.base + "float"
  val Decimal: String      = Xsd.base + "decimal"
  val Boolean: String      = Xsd.base + "boolean"
  val Date: String         = Xsd.base + "date"
  val Time: String         = Xsd.base + "time"
  val DateTime: String     = Xsd.base + "dateTime"
  val DateTimeOnly: String = Namespace.Shapes.base + "dateTimeOnly"
  val File: String         = Namespace.Shapes.base + "file"
  val Byte: String         = Xsd.base + "byte"
  val Binary: String       = Xsd.base + "base64Binary"
  val Password: String     = Namespace.Shapes.base + "password"
  val Any: String          = Xsd.base + "anyType"
  val AnyUri: String       = Xsd.base + "anyURI"
  val Nil: String          = Xsd.base + "nil"

  /** Return dataType qualified with Xsd namespace. */
  def apply(dataType: String): String = dataType match {
    case "string"       => String
    case "integer"      => Integer
    case "number"       => Number
    case "long"         => Long
    case "double"       => Double
    case "float"        => Float
    case "decimal"      => Decimal
    case "boolean"      => Boolean
    case "date"         => Date
    case "time"         => Time
    case "dateTime"     => DateTime
    case "dateTimeOnly" => DateTimeOnly
    case "file"         => File
    case "byte"         => Byte
    case "base64Binary" => Binary
    case "password"     => Password
    case "anyType"      => Any
    case "anyUri"       => AnyUri
    case "nil"          => Nil
    case _              => Xsd.base + dataType
  }
}
