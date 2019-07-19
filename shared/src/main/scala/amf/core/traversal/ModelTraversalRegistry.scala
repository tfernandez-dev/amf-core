package amf.core.traversal

import java.util.UUID

import amf.core.model.domain.{RecursiveShape, Shape}

import scala.collection.mutable

case class ModelTraversalRegistry() {

  private val backUps: mutable.Map[UUID, Set[String]]          = mutable.Map()
  private val ids: mutable.Set[String]                         = mutable.Set()
  private var whiteList: Set[String]                           = Set()
  private val whiteListBackUps: mutable.Map[UUID, Set[String]] = mutable.Map()

  private var allowedCycleClasses
  : Seq[Class[_]]                              = Seq() // i cant do it inmutable for the modularization (i cant see UnresolvedShape from here)
  private var stepOverFieldId: String => Boolean = (_: String) => false

  def withStepOverFunc(fn: String => Boolean): this.type = {
    stepOverFieldId = fn
    this
  }

  def withAllowedCyclesInstances(classes: Seq[Class[_]]): this.type = {
    allowedCycleClasses = classes
    this
  }

  def resetStepOverFun(): this.type = {
    stepOverFieldId = (_: String) => false
    this
  }

  def +(id: String): this.type = {
    ids += id
    this
  }

  def has(shape: Shape): Boolean =
    (!allowedCycleClasses.contains(shape.getClass)) && ids.contains(shape.id)

  def avoidError(id: String): Boolean = whiteList.contains(id)

  def avoidError(r: RecursiveShape, checkId: Option[String] = None): Boolean =
    avoidError(r.id) || avoidError(r.fixpoint.option().getOrElse("")) || (checkId.isDefined && avoidError(checkId.get))

  def hasId(id: String): Boolean = ids.contains(id)

  def canTravers(id: String): Boolean = !stepOverFieldId(id)

  private def push(): UUID = {
    val id = generateSha()
    backUps.put(id, ids.clone().toSet)
    id
  }

  def runWithIgnoredId(fnc: () => Shape, shapeId: String): Shape = runWithIgnoredIds(fnc, Set(shapeId))

  def runWithIgnoredIds(fnc: () => Shape, shapeIds: Set[String]): Shape = {
    val id = generateSha()
    whiteListBackUps.put(id, whiteList.toSet) // copy the whiteList set
    whiteList = whiteList ++ shapeIds
    val expanded = runPushed(_ => fnc())
    whiteList = whiteListBackUps(id)
    whiteListBackUps.remove(id)
    expanded
  }

  def recursionAllowed(fnc: () => Shape, shapeId: String): Shape = {
    val actual = ids.toSet + shapeId
    runWithIgnoredIds(fnc, actual)
  }

  def runPushed[T](fnc: ModelTraversalRegistry => T): T = {
    val uuid    = push()
    val element = fnc(this)
    ids.clear()
    ids ++= backUps(uuid)
    backUps.remove(uuid)
    element
  }

  def generateSha(): UUID = UUID.randomUUID()

}
