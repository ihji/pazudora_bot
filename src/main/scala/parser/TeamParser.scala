package parser

import data.{UserMonster, Team}
import db.MonsterDB

/**
  * Created by ihji on 3/6/16.
  */
object TeamParser {
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
      if(pair.length != 2 && pair.length != 3) Left("알 수 없는 형식입니다: "+name)
      else {
        val (name, pluses) = pair.length match {
          case 2 => pair(1) match {
            case "kr" => (s"${pair(0)}+kr", "0")
            case "jp" => (s"${pair(0)}+jp", "0")
            case _ => (pair(0), pair(1))
          }
          case 3 => (s"${pair(0)}+${pair(1)}", pair(2))
          case _ => ("", "")
        }
        val (hpPlus, atkPlus, rcvPlus) = {
          val splitted = pluses.split('/').map{_.trim}
          splitted.length match {
            case 3 => (splitted(0),splitted(1),splitted(2))
            case 1 =>
              if(splitted(0) == "297") ("99","99","99")
              else ("0",splitted(0),"0")
            case _ => ("","","")
          }
        }
        try {
          db.searchMonster(name).left.map{ x => s"잘못된 입력입니다 ($name): $x" }.right.map{UserMonster(_,hpPlus.toInt,atkPlus.toInt,rcvPlus.toInt)}
        } catch {
          case e : NumberFormatException => Left("숫자가 아닙니다: "+pluses)
        }
      }
    } else {
      db.searchMonster(name).left.map{ x => s"잘못된 입력입니다 ($name): $x" }.right.map{UserMonster(_,0,0,0)}
    }
  }
}
