package amf.core.model.domain

import amf.core.parser.Annotations

import scala.collection.mutable

/**
  * Created by pedro.colunga on 8/15/17.
  */
case class AmfArray(var values: Seq[AmfElement], annotations: Annotations = new Annotations()) extends AmfElement {

  def +=(value: AmfElement): Unit = {
    values = values :+ value
  }

  def scalars: Seq[AmfScalar] = values collect { case s: AmfScalar => s }

  override private[amf] def cloneElement(branch: mutable.Map[Int, AmfObject]): AmfArray =
    AmfArray(values.map(_.cloneElement(branch)), annotations.copy())
}
