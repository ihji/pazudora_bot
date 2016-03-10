package sem

import data.LeaderSkill.Mag
import data.{Monster, UserMonster, Input, Team}
import sem.DamageSimulator.Damage

/**
  * Created by heejong.lee on 3/9/16.
  */
class DamageSimulator(team: Team) {
  def run(input: Input): Map[UserMonster,Damage] = {
    team.toSeq.foldLeft(Map.empty[UserMonster,Damage]) {
      case (map,mon) =>
        val baseDamage = new Damage(0,0,0,0,0)
        val damageEquation =
          multiplyDrops(input,mon)_ andThen
            multiplyCombos(input) andThen
            multiplyRowEnhance(input,team) andThen
            multiplyLeaderSkill(input,team,mon)
        val finalDamage = damageEquation(baseDamage)
        map + (mon -> finalDamage)
    }
  }
  def getDamageString(input: Input) : String = {
    val map = run(input)
    val buf = new StringBuffer
    val (totalDamage,maxDamage) = team.toSeq.foldLeft(Damage(0,0,0,0,0),Damage(0,0,0,0,0)){
      case ((total,max),m) =>
        buf.append(s"*${m.toString}*: ${map(m).toString}\n")
        (total + map(m), max max map(m))
    }
    buf.append(s"*최대*: ${maxDamage.toString}\n")
    buf.append(s"*종합*: ${totalDamage.toString}")
    buf.toString
  }
  def multiplyDrops(input: Input, mon: UserMonster)(d: Damage) : Damage = {
    val (mainAttr,subAttr) = mon.mon.element
    val baseAtk = mon.getAtk
    println(s"$mon ${mon.mon.element} 공격력 $baseAtk ")
    input.combo.foldLeft(d) {
      (damage,set) =>
        val drop = 1 + 0.25*(set.num - 3)
        val coef =
          (mainAttr.toDrop == set.kind, subAttr.exists{_.toDrop == set.kind}) match {
            case (true,true) => 1.1
            case (true,false) => 1.0
            case (false,true) => 1.0 / 3.0
            case (false,false) => 0
          }
        val twoway = if(set.num == 4) mon.mon.awokenSkill.count{_ == Monster.TwoWayAtk} * 1.5 else 1
        val finalMag = drop * coef * twoway
        val finalAtk = Math.ceil(Math.ceil(baseAtk * drop * coef) * twoway)
        println(s"$set 드롭배수 $drop 주부속 $coef 투웨이배수 $twoway 최종배수 $finalMag 최종공격력 $finalAtk")
        damage.add(set.kind,finalAtk)
    }
  }
  def multiplyCombos(input: Input)(d: Damage) : Damage = {
    val combo = 1 + 0.25*(input.combo.length - 1)
    println(s"콤보배수 $combo")
    d.map{x => Math.ceil(x * combo)}
  }
  def multiplyRowEnhance(input: Input, team: Team)(d: Damage) : Damage = {
    def countAwokenSkills(t: Team, enh: Monster.AwokenSkill) : Int = {
      t.toSeq.map{_.mon.awokenSkill.count{_ == enh}}.sum
    }
    def countRowDrop(in: Input, d: Input.Drop) : Int ={
      in.combo.count{x => x.kind == d && x.isRow}
    }
    val fireRowEn = 1 + (countAwokenSkills(team, Monster.FireEn) * 0.1 * countRowDrop(input,Input.Fire))
    val waterRowEn = 1 + (countAwokenSkills(team, Monster.WaterEn) * 0.1 * countRowDrop(input,Input.Water))
    val woodRowEn = 1 + (countAwokenSkills(team, Monster.WoodEn) * 0.1 * countRowDrop(input,Input.Wood))
    val lightRowEn = 1 + (countAwokenSkills(team, Monster.LightEn) * 0.1 * countRowDrop(input,Input.Light))
    val darkRowEn = 1 + (countAwokenSkills(team, Monster.DarkEn) * 0.1 * countRowDrop(input,Input.Dark))
    println(s"횡강배수 불 $fireRowEn 물 $waterRowEn 나무 $woodRowEn 빛 $lightRowEn 어둠 $darkRowEn")
    d.copy(
      fireDamage = Math.round(d.fireDamage * fireRowEn),
      waterDamage = Math.round(d.waterDamage * waterRowEn),
      woodDamage = Math.round(d.woodDamage * woodRowEn),
      lightDamage = Math.round(d.lightDamage * lightRowEn),
      darkDamage = Math.round(d.darkDamage * darkRowEn)
    )
  }
  def multiplyLeaderSkill(input: Input, team: Team, mon: UserMonster)(d: Damage) : Damage = {
    val leaderMag = team.leader.mon.lSkill.map{_.getAtkMag(input,team,mon.mon)}.getOrElse(Mag.identity)
    val friendMag = team.friend.mon.lSkill.map{_.getAtkMag(input,team,mon.mon)}.getOrElse(Mag.identity)
    val finalMag = leaderMag * friendMag
    println(s"내리더 $leaderMag 친구리더 $friendMag")
    println(s"리더스킬 최종: $finalMag")
    d.copy(
      fireDamage = Math.round(d.fireDamage * finalMag.fire),
      waterDamage = Math.round(d.waterDamage * finalMag.water),
      woodDamage = Math.round(d.woodDamage * finalMag.wood),
      lightDamage = Math.round(d.lightDamage * finalMag.light),
      darkDamage = Math.round(d.darkDamage * finalMag.dark)
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
}

object DamageSimulator {
  case class Damage(fireDamage: Double, waterDamage: Double, woodDamage: Double, lightDamage: Double, darkDamage: Double, allAttack: Boolean = false) {
    override def toString = {
      s"불 $fireDamage 물 $waterDamage 나무 $woodDamage 빛 $lightDamage 어둠 $darkDamage"
    }
    def +(other: Damage) : Damage = {
      Damage(fireDamage+other.fireDamage,waterDamage+other.waterDamage,woodDamage+other.woodDamage,lightDamage+other.lightDamage,darkDamage+other.darkDamage)
    }
    def max(other: Damage) : Damage = {
      Damage(fireDamage max other.fireDamage,waterDamage max other.waterDamage,woodDamage max other.woodDamage,lightDamage max other.lightDamage,darkDamage max other.darkDamage)
    }
    def add(kind: Input.Drop, damage: Double) : Damage = {
      kind match {
        case Input.Fire => this.copy(fireDamage = fireDamage + damage)
        case Input.Water => this.copy(waterDamage = waterDamage + damage)
        case Input.Wood => this.copy(woodDamage = woodDamage + damage)
        case Input.Light => this.copy(lightDamage = lightDamage + damage)
        case Input.Dark => this.copy(darkDamage = darkDamage + damage)
        case _ => this
      }
    }
    def map(f: Double => Double) : Damage = {
      this.copy(fireDamage = f(fireDamage), waterDamage = f(waterDamage), woodDamage = f(woodDamage), lightDamage = f(lightDamage), darkDamage = f(darkDamage))
    }
  }
}

/*
Input:
/calc 2507+99, 2239, 2013+99, 2396+99, 893+1, 2507+99 = 어둠 1횡

Correct result:
각성 신마왕 루시퍼 체력+0 공격+99 회복+0 불 0.0 물 0.0 나무 0.0 빛 0.0 어둠 120624.0
요람의 저승신 페르세포네 체력+0 공격+0 회복+0 불 0.0 물 0.0 나무 0.0 빛 0.0 어둠 76764.0
각성 아누비스 체력+0 공격+99 회복+0 불 0.0 물 0.0 나무 0.0 빛 0.0 어둠 105927.0
각성 로키 체력+0 공격+99 회복+0 불 0.0 물 0.0 나무 0.0 빛 0.0 어둠 123839.0
단죄의 저승신 페르세포네 체력+0 공격+1 회복+0 불 0.0 물 0.0 나무 0.0 빛 0.0 어둠 86802.0
각성 신마왕 루시퍼 체력+0 공격+99 회복+0 불 0.0 물 0.0 나무 0.0 빛 0.0 어둠 120624.0
종합 불 0.0 물 0.0 나무 0.0 빛 0.0 어둠 634580.0
 */