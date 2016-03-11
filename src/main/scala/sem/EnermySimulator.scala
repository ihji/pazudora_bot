package sem

import data.{Monster, UserMonster, Enermy, Team}
import sem.DamageSimulator.Damage

/**
  * Created by heejong.lee on 3/10/16.
  */
class EnermySimulator(team: Team, damageMap: Map[UserMonster,Damage]) {
  def getActualDamageMap(e: Enermy) : Map[UserMonster,Damage] = {
    damageMap.map{
      case (mon,d) =>
        (mon, (modifyByElement(e)_ andThen modifyByKiller(e,mon) andThen modifyByDep(e))(d))
    }
  }
  private def getStrongWeakElementAgainst(e: Monster.Element) : (Monster.Element,Option[Monster.Element]) = {
    e match {
      case Monster.Fire => (Monster.Water,Some(Monster.Wood))
      case Monster.Water => (Monster.Wood,Some(Monster.Fire))
      case Monster.Wood => (Monster.Fire,Some(Monster.Water))
      case Monster.Light => (Monster.Dark,None)
      case Monster.Dark => (Monster.Light,None)
    }
  }
  private def mapElement(e: Monster.Element, f: Double => Double)(d: Damage) : Damage = {
    e match {
      case Monster.Fire => d.copy(fireDamage = f(d.fireDamage))
      case Monster.Water => d.copy(waterDamage = f(d.waterDamage))
      case Monster.Wood => d.copy(woodDamage = f(d.woodDamage))
      case Monster.Light => d.copy(lightDamage = f(d.lightDamage))
      case Monster.Dark => d.copy(darkDamage = f(d.darkDamage))
    }
  }
  def modifyByElement(e: Enermy)(d: Damage) : Damage = {
    val (twiceElem,halfElem) = getStrongWeakElementAgainst(e.elem)
    (mapElement(twiceElem, x => x * 2)_ andThen halfElem.map{mapElement(_, x => x / 2)_}.getOrElse{x: Damage => x})(d)
  }
  def modifyByDep(e: Enermy)(d: Damage) : Damage = {
    d.map{x => if(x == 0) 0 else x - e.dep max 1}
  }
  private def findKiller(ty: Monster.Type) : Option[Monster.AwokenSkill] = {
    ty match {
      case Monster.God => Some(Monster.GodKiller)
      case Monster.Devil => Some(Monster.DevilKiller)
      case Monster.Dragon => Some(Monster.DragonKiller)
      case Monster.Machine => Some(Monster.MachineKiller)
      case _ => None
    }
  }
  def modifyByKiller(e: Enermy, m: UserMonster)(d: Damage) : Damage = {
    e.tys.flatMap{findKiller}.foldLeft(d){
      case (damage, killer) =>
        val num = m.mon.awokenSkill.count{_ == killer}
        if(num == 0) damage
        else (0 until num).foldLeft(damage){case (x,_) => x.map{_ * 3}}
    }
  }
}
