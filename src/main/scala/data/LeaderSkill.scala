package data

import data.LeaderSkill._

/**
  * Created by heejong.lee on 3/7/16.
  */
class LeaderSkill(val name: String, val krDesc: String, val enDesc: String) {
  var attrCond : Map[Monster.Element,Double] = Map.empty
  var typeCond : Map[Monster.Type,Double] = Map.empty
  var comboCond : Option[ComboCond] = None
  var numberCond : Option[NumberCond] = None
  var fixedDropCond : Option[FixedDropCond] = None
  var flexDropCond : Option[FlexDropCond] = None

  def getAtkMag(input: Input, team: Team, mon: Monster) : Mags = {
    val attrMag = attrCond.getOrElse(mon.element._1,1.0) max mon.element._2.map{attrCond.getOrElse(_,1.0)}.getOrElse(1.0)
    val typeMag = typeCond.getOrElse(mon.ty._1,1.0) max mon.ty._2.map{typeCond.getOrElse(_,1.0)}.getOrElse(1.0) max mon.ty._3.map{typeCond.getOrElse(_,1.0)}.getOrElse(1.0)
    val comboMag = comboCond.map{ c =>
      val combo = input.combo.length
      if(combo >= c.endCombo) c.endMag
      else if(combo < c.startCombo) 1.0
      else {
        val comboMap = ((c.startCombo until c.endCombo) zip (c.startMag until c.endMag by c.step)).toMap
        comboMap.getOrElse(combo,1.0)
      }
    }.getOrElse(1.0)
    val numberMag = numberCond.map{ n =>
      val number = (input.combo.filter{x => n.drop(x.kind)}.map{_.num} :+ 0).max
      if(number >= n.endNumber) n.endMag
      else if(number < n.startNumber) 1.0
      else {
        val numberMap = ((n.startNumber until n.endNumber) zip (n.startMag until n.endMag by n.step)).toMap
        numberMap.getOrElse(number,1.0)
      }
    }.getOrElse(1.0)
    val fixedDropMag = fixedDropCond.map{ f =>
      if(f.drops.subsetOf(input.combo.map{_.kind}.toSet)) f.mag
      else 1.0
    }.getOrElse(1.0)
    val flexDropMag = flexDropCond.map{ f =>
      val matchedSet = input.combo.map{_.kind}.toSet
      val resultOpt = f.drops.subsets.filter{x => x.size >= f.startNum && x.size <= f.endNum}.find(_ == matchedSet)
      if(resultOpt.isEmpty) 1.0
      else {
        val numMap = ((f.startNum to f.endNum) zip (f.startMag to f.endMag by f.step)).toMap
        numMap.getOrElse(matchedSet.size, 1.0)
      }
    }.getOrElse(1.0)
    println(s"리더스킬 속성배수 $attrMag 타입배수 $typeMag 콤보배수 $comboMag 이어붙이기배수 $numberMag 고정색배수 $fixedDropMag 자유색배수 $flexDropMag")
    val noCondFinalMag = attrMag * typeMag
    val condFinalMag = comboMag * numberMag * fixedDropMag * flexDropMag
    Mags(Mag(noCondFinalMag,noCondFinalMag,noCondFinalMag,noCondFinalMag,noCondFinalMag),Mag(condFinalMag,condFinalMag,condFinalMag,condFinalMag,condFinalMag))
  }

  override def toString = {
    val buf = new StringBuffer
    if(attrCond.nonEmpty) {
      attrCond.foreach{ case (elem, mag) => buf.append(s"$elem 속성 $mag 배.")}
      buf.append("\n")
    }
    if(typeCond.nonEmpty) {
      typeCond.foreach{ case (ty, mag) => buf.append(s"$ty 타입 $mag 배.")}
      buf.append("\n")
    }
    if(comboCond.nonEmpty) {
      buf.append(s"${comboCond.get.startCombo} 콤보 ${comboCond.get.startMag} 배 부터 ${comboCond.get.step} 배씩 증가하여 최대 ${comboCond.get.endCombo} 콤보 ${comboCond.get.endMag} 배.")
      buf.append("\n")
    }
    if(numberCond.nonEmpty) {
      buf.append(s"${numberCond.get.drop.mkString(" 또는 ")} 드롭을 ${numberCond.get.startNumber} 개 이어 붙여 ${numberCond.get.startMag} 배 부터 ${numberCond.get.step} 배씩 증가하여 최대 ${numberCond.get.endNumber} 개 ${numberCond.get.endMag} 배.")
      buf.append("\n")
    }
    if(fixedDropCond.nonEmpty) {
      buf.append(s"${fixedDropCond.get.drops.mkString(",")} 동시 공격시 ${fixedDropCond.get.mag} 배")
      buf.append("\n")
    }
    if(flexDropCond.nonEmpty) {
      buf.append(s"${flexDropCond.get.drops.mkString(",")} 중 ${flexDropCond.get.startNum} 개 동시 공격시 ${flexDropCond.get.startMag} 배 부터 ${flexDropCond.get.step} 배씩 증가하여 최대 ${flexDropCond.get.endNum} 개 ${flexDropCond.get.endMag} 배.")
      buf.append("\n")
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
  case class Mag(fire: Double, water: Double, wood: Double, light: Double, dark: Double) {
    override def toString = s"불 ${fire}배 물 ${water}배 나무 ${wood}배 빛 ${light}배 어둠 ${dark}배"
    def *(other: Mag) : Mag = Mag(fire*other.fire,water*other.water,wood*other.wood,light*other.light,dark*other.dark)
  }
  object Mag {
    val identity = Mag(1.0,1.0,1.0,1.0,1.0)
  }
  case class Mags(noCond: Mag, cond: Mag)
  object Mags {
    val identity = Mags(Mag.identity, Mag.identity)
  }
}