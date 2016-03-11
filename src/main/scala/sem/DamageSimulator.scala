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
          multiplyDrops(input,team,mon)_ andThen
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
  def multiplyDrops(input: Input, team: Team, mon: UserMonster)(d: Damage) : Damage = {
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
        val finalMag = drop * coef * dropEnh * twoway
        val finalAtk = Math.ceil(Math.ceil(baseAtk * drop * coef * dropEnh) * twoway)
        println(s"$set 드롭배수 $drop 주부속 $coef 드롭강화배수 $dropEnh 투웨이배수 $twoway 최종배수 $finalMag 최종공격력 $finalAtk")
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
      fireDamage = Math.round(Math.round(d.fireDamage * leaderMag.fire) * friendMag.fire),
      waterDamage = Math.round(Math.round(d.waterDamage * leaderMag.water) * friendMag.water),
      woodDamage = Math.round(Math.round(d.woodDamage * leaderMag.wood) * friendMag.wood),
      lightDamage = Math.round(Math.round(d.lightDamage * leaderMag.light) * friendMag.light),
      darkDamage = Math.round(Math.round(d.darkDamage * leaderMag.dark) * friendMag.dark)
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
  case class Damage(fireDamage: Double, waterDamage: Double, woodDamage: Double, lightDamage: Double, darkDamage: Double, allAttack: Boolean = false) {
    private def friendly(d: Double) : String = java.text.NumberFormat.getIntegerInstance().format(d.toInt)
    override def toString = {
      Seq(
        if(fireDamage != 0) Some("불 "+friendly(fireDamage)) else None,
        if(waterDamage != 0) Some("물 "+friendly(waterDamage)) else None,
        if(woodDamage != 0) Some("나무 "+friendly(woodDamage)) else None,
        if(lightDamage != 0) Some("빛 "+friendly(lightDamage)) else None,
        if(darkDamage != 0) Some("어둠 "+friendly(darkDamage)) else None
      ).flatten.mkString(" ")
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
/calc 2507+99, 2239, 2013+99, 2396+99, 893+1, 2507+99 = 어둠 1횡 각 2강화

Correct result:
각성 신마왕 루시퍼 체+0 공+99 회+0: 어둠 135,091
요람의 저승신 페르세포네 체+0 공+0 회+0: 어둠 85,982
각성 아누비스 체+0 공+99 회+0: 어둠 118,656
각성 로키 체+0 공+99 회+0: 어둠 138,700
단죄의 저승신 페르세포네 체+0 공+1 회+0: 어둠 97,234
각성 신마왕 루시퍼 체+0 공+99 회+0: 어둠 135,091
최대: 어둠 138,700
종합: 어둠 710,754
 */