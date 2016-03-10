package data

/**
  * Created by heejong.lee on 3/9/16.
  */
case class UserMonster(mon: Monster, plusHp: Int = 0, plusAtk: Int = 0, plusRev: Int = 0) {
  def getHp = mon.hp._2 + plusHp * 10
  def getAtk = mon.atk._2 + plusAtk * 5
  def getRev = mon.rev._2 + plusRev * 3
  override def toString = {
    s"${mon.krName} 체력+$plusHp 공격+$plusAtk 회복+$plusRev"
  }
}
