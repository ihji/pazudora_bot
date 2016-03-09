package data

import data.LeaderSkill.{FlexDropCond, FixedDropCond, NumberCond, ComboCond}

/**
  * Created by heejong.lee on 3/7/16.
  */
class LeaderSkill {
  var attrCond : Map[Monster.Element,Double] = Map.empty
  var typeCond : Map[Monster.Type,Double] = Map.empty
  var comboCond : Option[ComboCond] = None
  var numberCond : Option[NumberCond] = None
  var fixedDropCond : Option[FixedDropCond] = None
  var flexDropCond : Option[FlexDropCond] = None

  override def toString = {
    val buf = new StringBuffer
    if(attrCond.nonEmpty) {
      attrCond.foreach{ case (elem, mag) => buf.append(s"$elem 속성 $mag 배.\n")}
    }
    if(typeCond.nonEmpty) {
      typeCond.foreach{ case (ty, mag) => buf.append(s"$ty 타입 $mag 배.\n")}
    }
    if(comboCond.nonEmpty) {
      buf.append(s"${comboCond.get.startCombo} 콤보 ${comboCond.get.startMag} 배 부터 ${comboCond.get.step} 배씩 증가하여 최대 ${comboCond.get.endCombo} 콤보 ${comboCond.get.endMag} 배.")
    }
    if(numberCond.nonEmpty) {
      buf.append(s"${numberCond.get.drop.mkString(" 또는 ")} 드롭을 ${numberCond.get.startNumber} 개 이어 붙여 ${numberCond.get.startMag} 배 부터 ${numberCond.get.step} 배씩 증가하여 최대 ${numberCond.get.endNumber} 개 ${numberCond.get.endMag} 배.")
    }
    if(fixedDropCond.nonEmpty) {
      buf.append(s"${fixedDropCond.get.drops.mkString(",")} 동시 공격시 ${fixedDropCond.get.mag} 배")
    }
    if(flexDropCond.nonEmpty) {
      buf.append(s"${flexDropCond.get.drops.mkString(",")} 중 ${flexDropCond.get.startNum} 개 동시 공격시 ${flexDropCond.get.startMag} 배 부터 ${flexDropCond.get.step} 배씩 증가하여 최대 ${flexDropCond.get.endNum} 개 ${flexDropCond.get.endMag} 배.")
    }
    buf.toString
  }

  def addAttrCond(elems: Set[Monster.Element], mag: Double) = {
    elems.foreach{ e => attrCond += (e -> mag) }; this
  }
  def addTypeCond(tys: Set[Monster.Type], mag: Double) = {
    tys.foreach{ t => typeCond += (t -> mag) }; this
  }
  def addComboCond(startCombo: Int, startMag: Double) = {
    val combo = comboCond.getOrElse(ComboCond(startCombo,startMag,startCombo,startMag,0))
    comboCond = Some(combo.copy(startCombo = startCombo, startMag = startMag))
    this
  }
  def addExtraComboCond(step: Double, endCombo: Int, endMag: Double) = {
    val combo = comboCond.getOrElse(ComboCond(endCombo,endMag,endCombo,endMag,step))
    comboCond = Some(combo.copy(endCombo = endCombo, endMag = endMag, step = step))
    this
  }
  def addNumberCond(drop: Set[Input.Drop], startNumber: Int, startMag: Double) = {
    val number = numberCond.getOrElse(NumberCond(drop, startNumber,startMag,startNumber,startMag,0))
    numberCond = Some(number.copy(drop = drop, startNumber = startNumber, startMag = startMag))
    this
  }
  def addExtraNumberCond(step: Double, endNumber: Int, endMag: Double) = {
    val number = numberCond.getOrElse(NumberCond(Set(), endNumber,endMag,endNumber,endMag,step))
    numberCond = Some(number.copy(endNumber = endNumber, endMag = endMag, step = step))
    this
  }
  def addFixedDropCond(drops: Set[Input.Drop], mag: Double) = {
    fixedDropCond = Some(FixedDropCond(drops,mag))
    this
  }
  def addFlexDropCond(drops: Set[Input.Drop], startNum: Int, startMag: Double) = {
    val drop = flexDropCond.getOrElse(FlexDropCond(drops, startNum, startMag, startNum, startMag, 0))
    flexDropCond = Some(drop.copy(drops = drops, startNum = startNum, startMag = startMag))
    this
  }
  def addExtraFlexDropCond(endNum: Int, endMag: Double, step: Double) = {
    val initDrops =
      if(endNum == 5) Set[Input.Drop](Input.Fire, Input.Water, Input.Wood, Input.Light, Input.Dark)
      else if(endNum == 6) Set[Input.Drop](Input.Fire, Input.Water, Input.Wood, Input.Light, Input.Dark, Input.Heart)
      else Set.empty[Input.Drop]
    val drop = flexDropCond.getOrElse(FlexDropCond(initDrops, endNum, endMag, endNum, endMag, step))
    flexDropCond = Some(drop.copy(endNum = endNum, endMag = endMag, step = step))
    this
  }
}

object LeaderSkill {
  case class ComboCond(startCombo: Int, startMag: Double, endCombo: Int, endMag: Double, step: Double)
  case class NumberCond(drop: Set[Input.Drop], startNumber: Int, startMag: Double, endNumber: Int, endMag: Double, step: Double)
  case class FixedDropCond(drops: Set[Input.Drop], mag: Double)
  case class FlexDropCond(drops: Set[Input.Drop], startNum: Int, startMag: Double, endNum: Int, endMag: Double, step: Double)
}