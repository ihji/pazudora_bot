package data

import data.LeaderSkill.{NumberCond, ComboCond}

/**
  * Created by heejong.lee on 3/7/16.
  */
class LeaderSkill {
  var attrCond : Map[Monster.Element,Double] = Map.empty
  var typeCond : Map[Monster.Type,Double] = Map.empty
  var comboCond : Option[ComboCond] = None
  var numberCond : Option[NumberCond] = None
  var dropCond : Map[Set[Input.Drop],Double] = Map.empty

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
      buf.append(s"${numberCond.get.startNumber} 개 ${numberCond.get.startMag} 배 부터 ${numberCond.get.step} 배씩 증가하여 최대 ${numberCond.get.endNumber} 개 ${comboCond.get.endMag} 배.")
    }
    if(dropCond.nonEmpty) {
      buf.append("드롭 컨디션이 존재함.")
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
}

object LeaderSkill {
  case class ComboCond(startCombo: Int, startMag: Double, endCombo: Int, endMag: Double, step: Double)
  case class NumberCond(startNumber: Int, startMag: Double, endNumber: Int, endMag: Double, step: Double)
}