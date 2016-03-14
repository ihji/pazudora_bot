package data

import data.LeaderSkill._

/**
  * Created by heejong.lee on 3/7/16.
  */
class LeaderSkill(val name: String, val krDesc: String, val enDesc: String) {
  var attrCond : Map[Monster.Element,Double] = Map.empty
  var typeCond : Map[Monster.Type,Double] = Map.empty
  var comboCond : Option[ComboCond] = None
  var fixedComboCond : Option[FixedComboCond] = None
  var numberCond : Option[NumberCond] = None
  var fixedDropCond : Option[FixedDropCond] = None
  var flexDropCond : Option[FlexDropCond] = None
  var enhDropCond : Option[EnhDropCond] = None

  def getAtkMag(input: Input, team: Team, mon: Monster) : Mags = {
    val attrMag = Mag(attrCond.getOrElse(mon.element._1,1.0) max mon.element._2.map{attrCond.getOrElse(_,1.0)}.getOrElse(1.0))
    val typeMag = Mag(typeCond.getOrElse(mon.ty._1,1.0) max mon.ty._2.map{typeCond.getOrElse(_,1.0)}.getOrElse(1.0) max mon.ty._3.map{typeCond.getOrElse(_,1.0)}.getOrElse(1.0))
    val comboMag = comboCond.map{ c =>
      val combo = input.combo.length
      if(combo >= c.endCombo) Mag(c.endMag)
      else if(combo < c.startCombo) Mag.identity
      else {
        val comboMap = ((c.startCombo until c.endCombo) zip (c.startMag until c.endMag by c.step)).toMap
        Mag(comboMap.getOrElse(combo,1.0))
      }
    }.getOrElse(Mag.identity)
    val fixedComboMag = fixedComboCond.map{ c =>
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
      if(combo.nonEmpty) Mag(magMap.getOrElse(combo.max,1.0))
      else Mag.identity
    }.getOrElse(Mag.identity)
    val numberMag = numberCond.map{ n =>
      val number = (input.combo.filter{x => n.drop(x.kind)}.map{_.num} :+ 0).max
      if(number >= n.endNumber) Mag(n.endMag)
      else if(number < n.startNumber) Mag.identity
      else {
        val numberMap = ((n.startNumber until n.endNumber) zip (n.startMag until n.endMag by n.step)).toMap
        Mag(numberMap.getOrElse(number,1.0))
      }
    }.getOrElse(Mag.identity)
    val fixedDropMag = fixedDropCond.map{ f =>
      if(f.drops.subsetOf(input.combo.map{_.kind}.toSet)) Mag(f.mag)
      else Mag.identity
    }.getOrElse(Mag.identity)
    val flexDropMag = flexDropCond.map{ f =>
      val matchedSet = input.combo.map{_.kind}.toSet
      val resultOpt = f.drops.subsets.filter{x => x.size >= f.startNum && x.size <= f.endNum}.find(_ == matchedSet)
      if(resultOpt.isEmpty) Mag.identity
      else {
        val numMap = ((f.startNum to f.endNum) zip (f.startMag to f.endMag by f.step)).toMap
        Mag(numMap.getOrElse(matchedSet.size, 1.0))
      }
    }.getOrElse(Mag.identity)
    val enhDropMag = enhDropCond.flatMap{ e =>
      input.combo.find{x => x.num == e.num && x.numEnhanced >= e.enNum}.map{ d =>
        d.kind match {
          case Input.Fire => Mag.identity.copy(fire = e.mag)
          case Input.Water => Mag.identity.copy(water = e.mag)
          case Input.Wood => Mag.identity.copy(wood = e.mag)
          case Input.Light => Mag.identity.copy(light = e.mag)
          case Input.Dark => Mag.identity.copy(dark = e.mag)
          case _ => Mag.identity
        }
      }
    }.getOrElse(Mag.identity)
    println(s"리더스킬:\n속성배수 $attrMag\n타입배수 $typeMag\n콤보배수 $comboMag\n이어붙이기배수 $numberMag\n고정색배수 $fixedDropMag\n자유색배수 $flexDropMag\n강화드롭배수 $enhDropMag\n고정콤보배수 $fixedComboMag")
    val noCondFinalMag = attrMag * typeMag
    val condFinalMag = comboMag * numberMag * fixedDropMag * flexDropMag * enhDropMag * fixedComboMag
    Mags(noCondFinalMag,condFinalMag)
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
    if(fixedComboCond.nonEmpty) {
      buf.append(s"${fixedComboCond.get.startDrops.map{_.mkString(",")}.mkString(" 또는 ")} 콤보에서 ${fixedComboCond.get.startMag} 배 부터 ${fixedComboCond.get.step} 배씩 증가하여 최대 ${fixedComboCond.get.endDrops.map{_.mkString(",")}.mkString(" 또는 ")} 콤보에서 ${fixedComboCond.get.endMag} 배.")
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
    if(enhDropCond.nonEmpty) {
      buf.append(s"강화드롭이 ${enhDropCond.get.enNum} 개 이상 포함된 ${enhDropCond.get.num} 개의 드롭을 지우면 해당 속성 ${enhDropCond.get.mag} 배")
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
    val combo = comboCond.getOrElse(ComboCond(startCombo,startMag,startCombo,startMag,1))
    comboCond = Some(combo.copy(startCombo = startCombo, startMag = startMag))
    this
  }
  def addExtraComboCond(step: Double, endCombo: Int, endMag: Double) = {
    val combo = comboCond.getOrElse(ComboCond(endCombo,endMag,endCombo,endMag,step))
    comboCond = Some(combo.copy(endCombo = endCombo, endMag = endMag, step = step))
    this
  }
  def addFixedComboCond(drops: Set[Seq[Input.Drop]], startMag: Double) = {
    val fixedCombo = fixedComboCond.getOrElse(FixedComboCond(drops,drops,startMag,startMag,1))
    fixedComboCond = Some(fixedCombo.copy(startDrops = drops, startMag = startMag))
    this
  }
  def addExtraFixedComboCond(step: Double, endMag: Double, endDrops: Either[Set[Seq[Input.Drop]],Int]) = {
    val fixedCombo = fixedComboCond.getOrElse(FixedComboCond(Set(),Set(),endMag,endMag,step))
    val ed = endDrops match {
      case Left(x) => if(x.isEmpty) fixedCombo.startDrops else x
      case Right(num) => fixedCombo.startDrops.map{x => (0 until num).map{_ => x.head}}
    }
    fixedComboCond = Some(fixedCombo.copy(endDrops = ed.toSet, endMag = endMag, step = step))
    this
  }
  def addNumberCond(drop: Set[Input.Drop], startNumber: Int, startMag: Double) = {
    val number = numberCond.getOrElse(NumberCond(drop, startNumber,startMag,startNumber,startMag,1))
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
    val drop = flexDropCond.getOrElse(FlexDropCond(drops, startNum, startMag, startNum, startMag, 1))
    flexDropCond = Some(drop.copy(drops = drops, startNum = startNum, startMag = startMag))
    this
  }
  def addEnhDropCond(num: Int, enNum: Int, mag: Double) = {
    val enh = enhDropCond.getOrElse(EnhDropCond(num, enNum, mag))
    enhDropCond = Some(enh.copy(num = num, enNum = enNum, mag = mag))
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
  case class FixedComboCond(startDrops: Set[Seq[Input.Drop]], endDrops: Set[Seq[Input.Drop]], startMag: Double, endMag: Double, step: Double)
  case class NumberCond(drop: Set[Input.Drop], startNumber: Int, startMag: Double, endNumber: Int, endMag: Double, step: Double)
  case class FixedDropCond(drops: Set[Input.Drop], mag: Double)
  case class FlexDropCond(drops: Set[Input.Drop], startNum: Int, startMag: Double, endNum: Int, endMag: Double, step: Double)
  case class EnhDropCond(num: Int, enNum: Int, mag: Double)
  case class Mag(fire: Double, water: Double, wood: Double, light: Double, dark: Double) {
    override def toString = s"불 ${fire}배 물 ${water}배 나무 ${wood}배 빛 ${light}배 어둠 ${dark}배"
    def *(other: Mag) : Mag = Mag(fire*other.fire,water*other.water,wood*other.wood,light*other.light,dark*other.dark)
  }
  object Mag {
    def apply(d: Double) : Mag = Mag(d,d,d,d,d)
    val identity = Mag(1.0,1.0,1.0,1.0,1.0)
  }
  case class Mags(noCond: Mag, cond: Mag)
  object Mags {
    val identity = Mags(Mag.identity, Mag.identity)
  }
}