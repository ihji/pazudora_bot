package data

import data.LeaderSkill._
import db.PDXLeaderSkillParser

/**
  * Created by heejong.lee on 3/7/16.
  */
class LeaderSkill private(val name: String, val krDesc: String, val enDesc: String) extends PDXLeaderSkillParser {
  var atkAttrCond : Map[Monster.Element,Double] = Map.empty
  var atkTypeCond : Map[Monster.Type,Double] = Map.empty
  var atkComboCond : Option[AtkComboCond] = None
  var atkFixedComboCond : Option[AtkFixedComboCond] = None
  var atkNumberCond : Option[AtkNumberCond] = None
  var atkFixedDropCond : Option[AtkFixedDropCond] = None
  var atkFlexDropCond : Option[AtkFlexDropCond] = None
  var atkEnhDropCond : Option[AtkEnhDropCond] = None

  var hpAttrCond : Map[Monster.Element,Double] = Map.empty
  var revAttrCond : Map[Monster.Element,Double] = Map.empty

  var hpTypeCond : Map[Monster.Type,Double] = Map.empty
  var revTypeCond : Map[Monster.Type,Double] = Map.empty

  def getHpMag(mon: Monster) : Double = {
    val attrMag = hpAttrCond.getOrElse(mon.element._1,1.0) max mon.element._2.map{hpAttrCond.getOrElse(_,1.0)}.getOrElse(1.0)
    val typeMag = hpTypeCond.getOrElse(mon.ty._1,1.0) max mon.ty._2.map{hpTypeCond.getOrElse(_,1.0)}.getOrElse(1.0) max mon.ty._3.map{hpTypeCond.getOrElse(_,1.0)}.getOrElse(1.0)
    attrMag * typeMag
  }
  def getRevMag(mon: Monster) : Double = {
    val attrMag = revAttrCond.getOrElse(mon.element._1,1.0) max mon.element._2.map{revAttrCond.getOrElse(_,1.0)}.getOrElse(1.0)
    val typeMag = revTypeCond.getOrElse(mon.ty._1,1.0) max mon.ty._2.map{revTypeCond.getOrElse(_,1.0)}.getOrElse(1.0) max mon.ty._3.map{revTypeCond.getOrElse(_,1.0)}.getOrElse(1.0)
    attrMag * typeMag
  }
  def getAtkMag(input: Input, team: Team, mon: Monster) : AtkMags = {
    val teamElem = team.toSeq.flatMap{x => List(Some(x.mon.element._1.toDrop),x.mon.element._2.map{_.toDrop})}.flatten.toSet ++ Set(Input.Heart, Input.Jammer)
    val attackingElem = input.combo.map{_.kind}.filter{teamElem}.toSet
    val attrMag = AtkMag(atkAttrCond.getOrElse(mon.element._1,1.0) max mon.element._2.map{atkAttrCond.getOrElse(_,1.0)}.getOrElse(1.0))
    val typeMag = AtkMag(atkTypeCond.getOrElse(mon.ty._1,1.0) max mon.ty._2.map{atkTypeCond.getOrElse(_,1.0)}.getOrElse(1.0) max mon.ty._3.map{atkTypeCond.getOrElse(_,1.0)}.getOrElse(1.0))
    val comboMag = atkComboCond.map{ c =>
      val combo = input.combo.length
      if(combo >= c.endCombo) AtkMag(c.endMag)
      else if(combo < c.startCombo) AtkMag.identity
      else {
        val comboMap = ((c.startCombo until c.endCombo) zip (c.startMag until c.endMag by c.step)).toMap
        AtkMag(comboMap.getOrElse(combo,1.0))
      }
    }.getOrElse(AtkMag.identity)
    val fixedComboMag = atkFixedComboCond.map{ c =>
      def makeCountSeq(drop: Seq[Input.Drop]) : Seq[Int] =
        Seq(drop.count(_ == Input.Fire), drop.count(_ == Input.Water), drop.count(_ == Input.Wood),
            drop.count(_ == Input.Light), drop.count(_ == Input.Dark),
            drop.count(_ == Input.Heart), drop.count(_ == Input.Jammer))
      def makeDropSeq(count: Seq[Int]) : Seq[Input.Drop] =
        Seq(
          Array.fill(count(0))(Input.Fire).toSeq,Array.fill(count(1))(Input.Water).toSeq,Array.fill(count(2))(Input.Wood).toSeq,
          Array.fill(count(3))(Input.Light).toSeq,Array.fill(count(4))(Input.Dark).toSeq,
          Array.fill(count(5))(Input.Heart).toSeq,Array.fill(count(6))(Input.Jammer).toSeq
        ).flatten
      def getAllNextSeq(from: Seq[Int], to: Seq[Int]) : Seq[Seq[Int]] = {
        to.zip(from).zipWithIndex.flatMap{
          case ((x,y),idx) =>
            if(x - y > 0) {
              val next = from.updated(idx,y+1)
              getAllNextSeq(next, to) :+ next
            } else Seq()
        }
      }
      def matched(x: Input)(y: Seq[Input.Drop]) : Boolean = {
        x.combo.foldLeft(y){
          case (m,c) => m diff Seq(c.kind)
        }.isEmpty
      }
      val allSeq =
        (for(sd <- c.startDrops; ed <- c.endDrops; sq <- getAllNextSeq(makeCountSeq(sd), makeCountSeq(ed)).map{makeDropSeq}) yield sq) ++ c.startDrops
      val (minCombo, maxCombo) = (allSeq.map{_.length}.min, allSeq.map{_.length}.max)
      val magMap = (minCombo to maxCombo).zipAll(c.startMag to c.endMag by c.step, maxCombo, c.endMag).toMap
      val combo = allSeq.filter{matched(input)}.map{_.length}
      if(combo.nonEmpty) AtkMag(magMap.getOrElse(combo.max,1.0))
      else AtkMag.identity
    }.getOrElse(AtkMag.identity)
    val numberMag = atkNumberCond.map{ n =>
      val number = (input.combo.filter{x => n.drop(x.kind)}.map{_.num} :+ 0).max
      if(number >= n.endNumber) AtkMag(n.endMag)
      else if(number < n.startNumber) AtkMag.identity
      else {
        val numberMap = ((n.startNumber until n.endNumber) zip (n.startMag until n.endMag by n.step)).toMap
        AtkMag(numberMap.getOrElse(number,1.0))
      }
    }.getOrElse(AtkMag.identity)
    val fixedDropMag = atkFixedDropCond.map{ f =>
      if(f.drops.subsetOf(attackingElem)) AtkMag(f.mag)
      else AtkMag.identity
    }.getOrElse(AtkMag.identity)
    val flexDropMag = atkFlexDropCond.map{ f =>
      val resultOpt = f.drops.subsets.filter{x => x.size >= f.startNum && x.size <= f.endNum}.find(_ == attackingElem)
      if(resultOpt.isEmpty) AtkMag.identity
      else {
        val numMap = ((f.startNum to f.endNum) zip (f.startMag to f.endMag by f.step)).toMap
        AtkMag(numMap.getOrElse(attackingElem.size, 1.0))
      }
    }.getOrElse(AtkMag.identity)
    val enhDropMag = atkEnhDropCond.flatMap{ e =>
      input.combo.find{x => x.num == e.num && x.numEnhanced >= e.enNum}.map{ d =>
        d.kind match {
          case Input.Fire => AtkMag.identity.copy(fire = e.mag)
          case Input.Water => AtkMag.identity.copy(water = e.mag)
          case Input.Wood => AtkMag.identity.copy(wood = e.mag)
          case Input.Light => AtkMag.identity.copy(light = e.mag)
          case Input.Dark => AtkMag.identity.copy(dark = e.mag)
          case _ => AtkMag.identity
        }
      }
    }.getOrElse(AtkMag.identity)
    println(s"리더스킬:\n속성배수 $attrMag\n타입배수 $typeMag\n콤보배수 $comboMag\n이어붙이기배수 $numberMag\n고정색배수 $fixedDropMag\n자유색배수 $flexDropMag\n강화드롭배수 $enhDropMag\n고정콤보배수 $fixedComboMag")
    val noCondFinalMag = attrMag * typeMag
    val condFinalMag = comboMag * numberMag * fixedDropMag * flexDropMag * enhDropMag * fixedComboMag
    AtkMags(noCondFinalMag,condFinalMag)
  }

  override def toString = {
    val buf = new StringBuffer
    if(hpAttrCond.nonEmpty) {
      hpAttrCond.foreach{ case (elem, mag) => buf.append(s"$elem 속성 HP $mag 배.")}
      buf.append("\n")
    }
    if(hpTypeCond.nonEmpty) {
      hpTypeCond.foreach{ case (ty, mag) => buf.append(s"$ty 타입 HP $mag 배.")}
      buf.append("\n")
    }
    if(revAttrCond.nonEmpty) {
      revAttrCond.foreach{ case (elem, mag) => buf.append(s"$elem 속성 회복 $mag 배.")}
      buf.append("\n")
    }
    if(revTypeCond.nonEmpty) {
      revTypeCond.foreach{ case (ty, mag) => buf.append(s"$ty 타입 회복 $mag 배.")}
      buf.append("\n")
    }
    if(atkAttrCond.nonEmpty) {
      atkAttrCond.foreach{ case (elem, mag) => buf.append(s"$elem 속성 공격 $mag 배.")}
      buf.append("\n")
    }
    if(atkTypeCond.nonEmpty) {
      atkTypeCond.foreach{ case (ty, mag) => buf.append(s"$ty 타입 공격 $mag 배.")}
      buf.append("\n")
    }
    if(atkComboCond.nonEmpty) {
      buf.append(s"${atkComboCond.get.startCombo} 콤보 ${atkComboCond.get.startMag} 배 부터 ${atkComboCond.get.step} 배씩 증가하여 최대 ${atkComboCond.get.endCombo} 콤보 ${atkComboCond.get.endMag} 배.")
      buf.append("\n")
    }
    if(atkFixedComboCond.nonEmpty) {
      buf.append(s"${atkFixedComboCond.get.startDrops.map{_.mkString(",")}.mkString(" 또는 ")} 콤보에서 ${atkFixedComboCond.get.startMag} 배 부터 ${atkFixedComboCond.get.step} 배씩 증가하여 최대 ${atkFixedComboCond.get.endDrops.map{_.mkString(",")}.mkString(" 또는 ")} 콤보에서 ${atkFixedComboCond.get.endMag} 배.")
      buf.append("\n")
    }
    if(atkNumberCond.nonEmpty) {
      buf.append(s"${atkNumberCond.get.drop.mkString(" 또는 ")} 드롭을 ${atkNumberCond.get.startNumber} 개 이어 붙여 ${atkNumberCond.get.startMag} 배 부터 ${atkNumberCond.get.step} 배씩 증가하여 최대 ${atkNumberCond.get.endNumber} 개 ${atkNumberCond.get.endMag} 배.")
      buf.append("\n")
    }
    if(atkFixedDropCond.nonEmpty) {
      buf.append(s"${atkFixedDropCond.get.drops.mkString(",")} 동시 공격시 ${atkFixedDropCond.get.mag} 배")
      buf.append("\n")
    }
    if(atkFlexDropCond.nonEmpty) {
      buf.append(s"${atkFlexDropCond.get.drops.mkString(",")} 중 ${atkFlexDropCond.get.startNum} 개 동시 공격시 ${atkFlexDropCond.get.startMag} 배 부터 ${atkFlexDropCond.get.step} 배씩 증가하여 최대 ${atkFlexDropCond.get.endNum} 개 ${atkFlexDropCond.get.endMag} 배.")
      buf.append("\n")
    }
    if(atkEnhDropCond.nonEmpty) {
      buf.append(s"강화드롭이 ${atkEnhDropCond.get.enNum} 개 이상 포함된 ${atkEnhDropCond.get.num} 개의 드롭을 지우면 해당 속성 ${atkEnhDropCond.get.mag} 배")
      buf.append("\n")
    }
    buf.toString
  }

  def addAtkAttrCond(elems: Set[Monster.Element], mag: Double) = {
    elems.foreach{ e => atkAttrCond += (e -> mag) }; this
  }
  def addAtkTypeCond(tys: Set[Monster.Type], mag: Double) = {
    tys.foreach{ t => atkTypeCond += (t -> mag) }; this
  }
  def addAtkComboCond(startCombo: Int, startMag: Double) = {
    val combo = atkComboCond.getOrElse(AtkComboCond(startCombo,startMag,startCombo,startMag,1))
    atkComboCond = Some(combo.copy(startCombo = startCombo, startMag = startMag))
    this
  }
  def addAtkExtraComboCond(step: Double, endCombo: Int, endMag: Double) = {
    val combo = atkComboCond.getOrElse(AtkComboCond(endCombo,endMag,endCombo,endMag,step))
    atkComboCond = Some(combo.copy(endCombo = endCombo, endMag = endMag, step = step))
    this
  }
  def addAtkFixedComboCond(drops: Set[Seq[Input.Drop]], startMag: Double) = {
    val fixedCombo = atkFixedComboCond.getOrElse(AtkFixedComboCond(drops,drops,startMag,startMag,1))
    atkFixedComboCond = Some(fixedCombo.copy(startDrops = drops, startMag = startMag))
    this
  }
  def addAtkExtraFixedComboCond(step: Double, endMag: Double, endDrops: Either[Set[Seq[Input.Drop]],Int]) = {
    val fixedCombo = atkFixedComboCond.getOrElse(AtkFixedComboCond(Set(),Set(),endMag,endMag,step))
    val ed = endDrops match {
      case Left(x) => if(x.isEmpty) fixedCombo.startDrops else x
      case Right(num) => fixedCombo.startDrops.map{x => (0 until num).map{_ => x.head}}
    }
    atkFixedComboCond = Some(fixedCombo.copy(endDrops = ed.toSet, endMag = endMag, step = step))
    this
  }
  def addAtkNumberCond(drop: Set[Input.Drop], startNumber: Int, startMag: Double) = {
    val number = atkNumberCond.getOrElse(AtkNumberCond(drop, startNumber,startMag,startNumber,startMag,1))
    atkNumberCond = Some(number.copy(drop = drop, startNumber = startNumber, startMag = startMag))
    this
  }
  def addAtkExtraNumberCond(step: Double, endNumber: Int, endMag: Double) = {
    val number = atkNumberCond.getOrElse(AtkNumberCond(Set(), endNumber,endMag,endNumber,endMag,step))
    atkNumberCond = Some(number.copy(endNumber = endNumber, endMag = endMag, step = step))
    this
  }
  def addAtkFixedDropCond(drops: Set[Input.Drop], mag: Double) = {
    atkFixedDropCond = Some(AtkFixedDropCond(drops,mag))
    this
  }
  def addAtkFlexDropCond(drops: Set[Input.Drop], startNum: Int, startMag: Double) = {
    val drop = atkFlexDropCond.getOrElse(AtkFlexDropCond(drops, startNum, startMag, startNum, startMag, 1))
    atkFlexDropCond = Some(drop.copy(drops = drops, startNum = startNum, startMag = startMag))
    this
  }
  def addAtkEnhDropCond(num: Int, enNum: Int, mag: Double) = {
    val enh = atkEnhDropCond.getOrElse(AtkEnhDropCond(num, enNum, mag))
    atkEnhDropCond = Some(enh.copy(num = num, enNum = enNum, mag = mag))
    this
  }
  def addAtkExtraFlexDropCond(endNum: Int, endMag: Double, step: Double) = {
    val initDrops =
      if(endNum == 5) Set[Input.Drop](Input.Fire, Input.Water, Input.Wood, Input.Light, Input.Dark)
      else if(endNum == 6) Set[Input.Drop](Input.Fire, Input.Water, Input.Wood, Input.Light, Input.Dark, Input.Heart)
      else Set.empty[Input.Drop]
    val drop = atkFlexDropCond.getOrElse(AtkFlexDropCond(initDrops, endNum, endMag, endNum, endMag, step))
    atkFlexDropCond = Some(drop.copy(endNum = endNum, endMag = endMag, step = step))
    this
  }
  def addHpAttrCond(elems: Set[Monster.Element], mag: Double) = {
    elems.foreach{ e => hpAttrCond += (e -> mag) }; this
  }
  def addRevAttrCond(elems: Set[Monster.Element], mag: Double) = {
    elems.foreach{ e => revAttrCond += (e -> mag) }; this
  }
  def addHpTypeCond(tys: Set[Monster.Type], mag: Double) = {
    tys.foreach{ t => hpTypeCond += (t -> mag) }; this
  }
  def addRevTypeCond(tys: Set[Monster.Type], mag: Double) = {
    tys.foreach{ t => revTypeCond += (t -> mag) }; this
  }
}

object LeaderSkill {
  def apply(name: String, krDesc: String, enDesc: String) : LeaderSkill = {
    new LeaderSkill(name,krDesc,enDesc).genLeaderSkill()
  }
  case class AtkComboCond(startCombo: Int, startMag: Double, endCombo: Int, endMag: Double, step: Double)
  case class AtkFixedComboCond(startDrops: Set[Seq[Input.Drop]], endDrops: Set[Seq[Input.Drop]], startMag: Double, endMag: Double, step: Double)
  case class AtkNumberCond(drop: Set[Input.Drop], startNumber: Int, startMag: Double, endNumber: Int, endMag: Double, step: Double)
  case class AtkFixedDropCond(drops: Set[Input.Drop], mag: Double)
  case class AtkFlexDropCond(drops: Set[Input.Drop], startNum: Int, startMag: Double, endNum: Int, endMag: Double, step: Double)
  case class AtkEnhDropCond(num: Int, enNum: Int, mag: Double)
  case class AtkMag(fire: Double, water: Double, wood: Double, light: Double, dark: Double) {
    override def toString = s"불 ${fire}배 물 ${water}배 나무 ${wood}배 빛 ${light}배 어둠 ${dark}배"
    def *(other: AtkMag) : AtkMag = AtkMag(fire*other.fire,water*other.water,wood*other.wood,light*other.light,dark*other.dark)
  }
  object AtkMag {
    def apply(d: Double) : AtkMag = AtkMag(d,d,d,d,d)
    val identity = AtkMag(1.0,1.0,1.0,1.0,1.0)
  }
  case class AtkMags(noCond: AtkMag, cond: AtkMag)
  object AtkMags {
    val identity = AtkMags(AtkMag.identity, AtkMag.identity)
  }
}