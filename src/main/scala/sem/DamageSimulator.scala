package sem

import data.LeaderSkill.{Mags, Mag}
import data.{Monster, UserMonster, Input, Team}
import sem.DamageSimulator.Damage

/**
  * Created by heejong.lee on 3/9/16.
  */
class DamageSimulator(team: Team) {
  def run(input: Input): Map[UserMonster,Damage] = {
    team.toSeq.foldLeft(Map.empty[UserMonster,Damage]) {
      case (map,mon) =>
        val baseDamage = Damage.empty
        val damageEquation =
          multiplyDrops(input,team,mon)_ andThen
            multiplyCombos(input) andThen
            multiplyRowEnhance(input,team) andThen
            multiplyLeaderSkill(input,team,mon)
        val finalDamage = damageEquation(baseDamage)
        map + (mon -> finalDamage)
    }
  }
  def getDamageString(map: Map[UserMonster,Damage]) : String = {
    val buf = new StringBuffer
    val (totalDamage,maxDamage) = team.toSeq.foldLeft(Damage.empty,Damage.empty){
      case ((total,max),m) =>
        buf.append(s"*${m.toString}*: ${map(m).toString}\n")
        (total + map(m), max max map(m))
    }
    buf.append(s"*최대*: ${maxDamage.toString(Math.max)}\n")
    buf.append(s"*종합*: ${totalDamage.toString(_+_)} ")
    buf.append(s"전체 ${Damage.friendly(totalDamage.sum)}")
    buf.toString
  }
  def multiplyDrops(input: Input, team: Team, mon: UserMonster)(d: Damage) : Damage = {
    val (mainAttr,subAttr) = mon.mon.element
    val baseAtk = mon.getAtk
    println(s"$mon ${mon.mon.element} 공격력 $baseAtk ")
    input.combo.foldLeft(d) {
      (damage,set) =>
        val drop = 1 + 0.25*(set.num - 3)
        val dropEnhanceCount =
          set.kind match {
            case Input.Fire => countAwokenSkills(team,Monster.FireDropEn)
            case Input.Water => countAwokenSkills(team,Monster.WaterDropEn)
            case Input.Wood => countAwokenSkills(team,Monster.WoodDropEn)
            case Input.Light => countAwokenSkills(team,Monster.LightDropEn)
            case Input.Dark => countAwokenSkills(team,Monster.DarkDropEn)
            case Input.Heart => countAwokenSkills(team,Monster.HeartDropEn)
            case _ => 0
          }
        val dropEnh = (1 + 0.06 * set.numEnhanced) * (if(set.numEnhanced != 0) 1 + 0.05 * dropEnhanceCount else 1)
        val twowayCount = mon.mon.awokenSkill.count{_ == Monster.TwoWayAtk}
        val twoway = if(set.num == 4 && twowayCount != 0) Array.fill(twowayCount)(1.5).product else 1
        val finalMag = drop * dropEnh * twoway
        def atk(base: Double) = Math.round(Math.ceil(base * drop * dropEnh) * twoway).toDouble

        val bases =
          (mainAttr.toDrop == set.kind, subAttr.exists{_.toDrop == set.kind}) match {
            case (true,true) => (baseAtk.toDouble, baseAtk * 0.1)
            case (true,false) => (baseAtk.toDouble, 0.0)
            case (false,true) => (0.0, baseAtk / 3.0)
            case (false,false) => (0.0, 0.0)
          }

        val finalAtk = (atk(bases._1), atk(bases._2))
        println(s"$set 드롭배수 $drop 드롭강화배수 $dropEnh 투웨이배수 $twoway 최종배수 $finalMag 최종공격력 $finalAtk")
        damage.add(set.kind,finalAtk)
    }
  }
  def multiplyCombos(input: Input)(d: Damage) : Damage = {
    val combo = 1 + 0.25*(input.combo.length - 1)
    println(s"콤보배수 $combo")
    d.map{x => Math.ceil(x * combo)}
  }
  def multiplyRowEnhance(input: Input, team: Team)(d: Damage) : Damage = {
    val fireRowEn = 1 + (countAwokenSkills(team, Monster.FireEn) * 0.1 * countRowDrop(input,Input.Fire))
    val waterRowEn = 1 + (countAwokenSkills(team, Monster.WaterEn) * 0.1 * countRowDrop(input,Input.Water))
    val woodRowEn = 1 + (countAwokenSkills(team, Monster.WoodEn) * 0.1 * countRowDrop(input,Input.Wood))
    val lightRowEn = 1 + (countAwokenSkills(team, Monster.LightEn) * 0.1 * countRowDrop(input,Input.Light))
    val darkRowEn = 1 + (countAwokenSkills(team, Monster.DarkEn) * 0.1 * countRowDrop(input,Input.Dark))
    println(s"횡강배수 불 $fireRowEn 물 $waterRowEn 나무 $woodRowEn 빛 $lightRowEn 어둠 $darkRowEn")
    d.copy(
      fireDamage = (Math.round(d.fireDamage._1 * fireRowEn), Math.round(d.fireDamage._2 * fireRowEn)),
      waterDamage = (Math.round(d.waterDamage._1 * waterRowEn), Math.round(d.waterDamage._2 * waterRowEn)),
      woodDamage = (Math.round(d.woodDamage._1 * woodRowEn), Math.round(d.woodDamage._2 * woodRowEn)),
      lightDamage = (Math.round(d.lightDamage._1 * lightRowEn), Math.round(d.lightDamage._2 * lightRowEn)),
      darkDamage = (Math.round(d.darkDamage._1 * darkRowEn), Math.round(d.darkDamage._2 * darkRowEn))
    )
  }
  def multiplyLeaderSkill(input: Input, team: Team, mon: UserMonster)(d: Damage) : Damage = {
    val leaderMag = team.leader.mon.lSkill.map{_.getAtkMag(input,team,mon.mon)}.getOrElse(Mags.identity)
    val friendMag = team.friend.mon.lSkill.map{_.getAtkMag(input,team,mon.mon)}.getOrElse(Mags.identity)
    val finalNoCondMag = leaderMag.noCond * friendMag.noCond
    val finalCondMag = leaderMag.cond * friendMag.cond
    println(s"기본배수 $finalNoCondMag 조건부배수 $finalCondMag")
    println(s"리더스킬 최종: ${finalNoCondMag * finalCondMag}")
    def roundUpMult(seq: Seq[Double])(damages: (Double, Double)) : (Double, Double) = {
      seq.foldLeft(damages) {
        case ((fst,snd),dam) => (Math.round(fst * dam), Math.round(snd * dam))
      }
    }
    d.copy(
      fireDamage = roundUpMult(Seq(leaderMag.cond.fire,friendMag.cond.fire,finalNoCondMag.fire))(d.fireDamage),
      waterDamage = roundUpMult(Seq(leaderMag.cond.water,friendMag.cond.water,finalNoCondMag.water))(d.waterDamage),
      woodDamage = roundUpMult(Seq(leaderMag.cond.wood,friendMag.cond.wood,finalNoCondMag.wood))(d.woodDamage),
      lightDamage = roundUpMult(Seq(leaderMag.cond.light,friendMag.cond.light,finalNoCondMag.light))(d.lightDamage),
      darkDamage = roundUpMult(Seq(leaderMag.cond.dark,friendMag.cond.dark,finalNoCondMag.dark))(d.darkDamage)
    )
  }
  private def equals(elem: Monster.Element, drop: Input.Drop) = {
    (elem,drop) match {
      case (Monster.Fire,Input.Fire) => true
      case (Monster.Water,Input.Water) => true
      case (Monster.Wood,Input.Wood) => true
      case (Monster.Light,Input.Light) => true
      case (Monster.Dark,Input.Dark) => true
      case _ => false
    }
  }
  private def countAwokenSkills(t: Team, enh: Monster.AwokenSkill) : Int = {
    t.toSeq.map{_.mon.awokenSkill.count{_ == enh}}.sum
  }
  private def countRowDrop(in: Input, d: Input.Drop) : Int = {
    in.combo.count{x => x.kind == d && x.isRow}
  }
}

object DamageSimulator {
  case class Damage(fireDamage: (Double,Double), waterDamage: (Double,Double), woodDamage: (Double,Double), lightDamage: (Double,Double), darkDamage: (Double,Double), mainAllAttack: Option[Monster.Element] = None, subAllAttack: Option[Monster.Element] = None) {
    import Damage.friendly
    override def toString = {
      Seq(
        if(fireDamage._1 != 0) Some("불 "+friendly(fireDamage._1)) else None,
        if(fireDamage._2 != 0) Some("불부 "+friendly(fireDamage._2)) else None,
        if(waterDamage._1 != 0) Some("물 "+friendly(waterDamage._1)) else None,
        if(waterDamage._2 != 0) Some("물부 "+friendly(waterDamage._2)) else None,
        if(woodDamage._1 != 0) Some("나무 "+friendly(woodDamage._1)) else None,
        if(woodDamage._2 != 0) Some("나무부 "+friendly(woodDamage._2)) else None,
        if(lightDamage._1 != 0) Some("빛 "+friendly(lightDamage._1)) else None,
        if(lightDamage._2 != 0) Some("빛부 "+friendly(lightDamage._2)) else None,
        if(darkDamage._1 != 0) Some("어둠 "+friendly(darkDamage._1)) else None,
        if(darkDamage._2 != 0) Some("어둠부 "+friendly(darkDamage._2)) else None
      ).flatten.mkString(" ")
    }
    def toString(f: (Double,Double) => Double) = {
      Seq(
        if(f(fireDamage._1, fireDamage._2) != 0) Some("불 "+friendly(f(fireDamage._1, fireDamage._2))) else None,
        if(f(waterDamage._1, waterDamage._2) != 0) Some("물 "+friendly(f(waterDamage._1, waterDamage._2))) else None,
        if(f(woodDamage._1, woodDamage._2) != 0) Some("나무 "+friendly(f(woodDamage._1, woodDamage._2))) else None,
        if(f(lightDamage._1, lightDamage._2) != 0) Some("빛 "+friendly(f(lightDamage._1, lightDamage._2))) else None,
        if(f(darkDamage._1, darkDamage._2) != 0) Some("어둠 "+friendly(f(darkDamage._1, darkDamage._2))) else None
      ).flatten.mkString(" ")
    }
    def sum : Double = {
      fireDamage._1 + fireDamage._2 +
      waterDamage._1 + waterDamage._2 +
      woodDamage._1 + woodDamage._2 +
      lightDamage._1 + lightDamage._2 +
      darkDamage._1 + darkDamage._2
    }
    def +(other: Damage) : Damage = {
      Damage(
        (fireDamage._1+other.fireDamage._1,fireDamage._2+other.fireDamage._2),
        (waterDamage._1+other.waterDamage._1,waterDamage._2+other.waterDamage._2),
        (woodDamage._1+other.woodDamage._1,woodDamage._2+other.woodDamage._2),
        (lightDamage._1+other.lightDamage._1,lightDamage._2+other.lightDamage._2),
        (darkDamage._1+other.darkDamage._1,darkDamage._2+other.darkDamage._2)
      )
    }
    def max(other: Damage) : Damage = {
      Damage(
        (fireDamage._1 max other.fireDamage._1, fireDamage._2 max other.fireDamage._2),
        (waterDamage._1 max other.waterDamage._1, waterDamage._2 max other.waterDamage._2),
        (woodDamage._1 max other.woodDamage._1, woodDamage._2 max other.woodDamage._2),
        (lightDamage._1 max other.lightDamage._1, lightDamage._2 max other.lightDamage._2),
        (darkDamage._1 max other.darkDamage._1, darkDamage._2 max other.darkDamage._2)
      )
    }
    def add(kind: Input.Drop, damage: (Double,Double)) : Damage = {
      kind match {
        case Input.Fire => this.copy(fireDamage = (fireDamage._1 + damage._1, fireDamage._2 + damage._2))
        case Input.Water => this.copy(waterDamage = (waterDamage._1 + damage._1, waterDamage._2 + damage._2))
        case Input.Wood => this.copy(woodDamage = (woodDamage._1 + damage._1, woodDamage._2 + damage._2))
        case Input.Light => this.copy(lightDamage = (lightDamage._1 + damage._1, lightDamage._2 + damage._2))
        case Input.Dark => this.copy(darkDamage = (darkDamage._1 + damage._1, darkDamage._2 + damage._2))
        case _ => this
      }
    }
    def map(f: Double => Double) : Damage = {
      this.copy(
        fireDamage = (f(fireDamage._1),f(fireDamage._2)),
        waterDamage = (f(waterDamage._1),f(waterDamage._2)),
        woodDamage = (f(woodDamage._1),f(woodDamage._2)),
        lightDamage = (f(lightDamage._1),f(lightDamage._2)),
        darkDamage = (f(darkDamage._1),f(darkDamage._2))
      )
    }
  }
  object Damage {
    val empty = Damage((0,0),(0,0),(0,0),(0,0),(0,0))
    def friendly(d: Double) : String = java.text.NumberFormat.getIntegerInstance().format(d.toInt)
  }
}