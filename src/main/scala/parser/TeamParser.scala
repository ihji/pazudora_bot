package parser

import data.{UserMonster, Team}
import db.{TIGSearch, MonsterDB}

/**
  * Created by ihji on 3/6/16.
  */
object TeamParser extends TIGSearch {
  val db = MonsterDB
  def parseTeam(team: String) : Either[String,Team] = {
    val t = team.split(",").toList.map{x => parseMonster(x.trim)}
    if(t.exists{_.isLeft}) Left(t.find{_.isLeft}.get.left.get)
    else if(t.length != 6) Left("팀은 6마리로 편성해야 합니다.")
    else if(t.exists{x => !sanityCheck(x.right.get)}) Left("+ 강화의 경우 0 이상 99 이하이어야 합니다.")
    else {
      val ts = t.map{_.right.get}
      Right(Team(ts(0),ts(1),ts(2),ts(3),ts(4),ts(5)))
    }
  }
  def sanityCheck(mon: UserMonster) : Boolean = {
    0 <= mon.plusAtk && mon.plusAtk <= 99 &&
    0 <= mon.plusHp && mon.plusHp <= 99 &&
    0 <= mon.plusRev && mon.plusRev <= 99
  }
  def parseMonster(name: String) : Either[String, UserMonster] = {
    if(name.contains("+")) {
      val pair = name.split('+').map{_.trim}
      if(pair.length != 2) Left("알 수 없는 형식입니다: "+name)
      else {
        val (name, plus) = (pair(0), pair(1))
        try {
          nameOrId(name).right.map { x => UserMonster(db.getMonster(x)).copy(plusAtk = plus.toInt) }
        } catch {
          case e : NumberFormatException => Left("숫자가 아닙니다: "+plus)
        }
      }
    } else {
      nameOrId(name).right.map{db.getMonster _ andThen (UserMonster(_,0,0,0))}
    }
  }
}
