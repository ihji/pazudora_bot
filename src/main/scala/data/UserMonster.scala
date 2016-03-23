package data

/**
  * Created by heejong.lee on 3/9/16.
  */
case class UserMonster(mon: Monster, plusHp: Int = 0, plusAtk: Int = 0, plusRev: Int = 0) {
  def getHp = mon.hp._2 + plusHp * 10 + mon.awokenSkill.count{_ == Monster.HpEn} * 200
  def getAtk = mon.atk._2 + plusAtk * 5 + mon.awokenSkill.count{_ == Monster.AtkEn} * 100
  def getRev = mon.rev._2 + plusRev * 3 + mon.awokenSkill.count{_ == Monster.RevEn} * 50
  override def toString = {
    val plus = Seq(
      if(plusHp != 0) Some(s"체+$plusHp") else None,
      if(plusAtk != 0) Some(s"공+$plusAtk") else None,
      if(plusRev != 0) Some(s"회+$plusRev") else None
    ).flatten.mkString(" ")
    s"${mon.krName} $plus"
  }
}
