package amf.core.errorhandling
import amf.core.validation.AMFValidationResult

import scala.collection.mutable

object StaticErrorCollector{

  private val errorsForUnitCount: mutable.Map[Int, Seq[AMFValidationResult]] = mutable.Map.empty

  def collect(result: AMFValidationResult, parserRun:Int): Unit = synchronized {
    errorsForUnitCount.get(parserRun) match {
      case Some(seq) => errorsForUnitCount.update(parserRun,  result +: seq)
      case None => errorsForUnitCount.put(parserRun,Seq(result))
    }
  }

  def clean(): Unit = errorsForUnitCount.clear()

  def getRun(parserRun:Int): Seq[AMFValidationResult] = errorsForUnitCount.getOrElse(parserRun, Nil)

}