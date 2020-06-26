package amf.core.remote

import amf.core.remote.Mimes._

/**
  * Syntax
  */
object Syntax {

  sealed trait Syntax {
    val extension: String
  }

  case object Yaml extends Syntax {
    override val extension: String = "yaml"
  }
  case object Json extends Syntax {
    override val extension: String = "json"
  }

  case object PlainText extends Syntax {
    override val extension: String = "txt"
  }

  private val yamlMimes = Set(`TEXT/YAML`, `TEXT/X-YAML`, `TEXT/VND.YAML`, `APPLICATION/YAML`, `APPLICATION/X-YAML`,
    `APPLICATION/RAML+YAML`, `APPLICATION/OPENAPI+YAML`, `APPLICATION/SWAGGER+YAML`, `APPLICATION/ASYNCAPI+YAML`,
    `APPLICATION/ASYNC+YAML`)

  private val jsonMimes = Set(`APPLICATION/JSON`, `APPLICATION/RAML+JSON`, `APPLICATION/OPENAPI+JSON`,
    `APPLICATION/SWAGGER+JSON`, `APPLICATION/ASYNCAPI+JSON`, `APPLICATION/ASYNC+JSON`)

  /** Attempt to resolve [[Syntax]] from [[Mimes]]. */
  def unapply(mime: Option[String]): Option[Syntax] = mime match {
    case Some(m) if yamlMimes.contains(m) => Some(Yaml)
    case Some(m) if jsonMimes.contains(m) => Some(Json)
    case _ => None
  }
}
