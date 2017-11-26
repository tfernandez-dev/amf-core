package amf.core.model.domain.templates

import amf.core.metamodel.domain.templates.ParametrizedDeclarationModel._
import amf.core.model.domain.DomainElement
import amf.core.parser.{Annotations, Fields}

abstract class ParametrizedDeclaration(fields: Fields, annotations: Annotations) extends DomainElement {
  def name: String                  = fields(Name)
  def target: String                = fields(Target)
  def variables: Seq[VariableValue] = fields(Variables)

  def withName(name: String): this.type                       = set(Name, name)
  def withTarget(target: String): this.type                   = set(Target, target)
  def withVariables(variables: Seq[VariableValue]): this.type = setArray(Variables, variables)

  override def adopted(parent: String): this.type = withId(parent + "/" + name)
}
