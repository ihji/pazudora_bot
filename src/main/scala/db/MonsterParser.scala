package db

import data.{LeaderSkill, Monster}
import data.Monster.{AwokenSkill, ActiveSkill}
import db.web.{PDXParser, TIGParser}

/**
  * Created by heejong.lee on 3/15/16.
  */
trait MonsterParser extends TIGParser with PDXParser {
  def parseMonster(id: MonsterID) : Monster = {
    val doc = getDocument(id)

    val rawName = getName(doc)
    val idx = rawName(0).drop(3).toInt
    val krName = rawName(1)
    val jpName = rawName(2)
    val ty = {
      val ts = rawName(3).split("/")
      ts.length match {
        case 3 =>
          (Monster.toType(ts(0).dropRight(2)), Some(Monster.toType(ts(1).dropRight(2))), Some(Monster.toType(ts(2).dropRight(2))))
        case 2 =>
          (Monster.toType(ts(0).dropRight(2)), Some(Monster.toType(ts(1).dropRight(2))), None)
        case 1 =>
          (Monster.toType(ts(0).dropRight(2)), None, None)
      }
    }
    val star = if(rawName(4).length == 1) 1 else rawName(4).drop(1).toInt

    val rawElement = getElementsStringFromUS(id)
    val element = {
      val es = rawElement.split("/")
      es.length match {
        case 2 =>
          (Monster.toElem(es(0)), Some(Monster.toElem(es(1))))
        case 1 =>
          (Monster.toElem(es(0)), None)
      }
    }
    val (thumbURL,picURL) = getPic(doc)

    val rawStat = getStat(doc)
    val statReg = """(\d+) \([\+-]\d+\)""".r
    val hpMatch = statReg.findFirstMatchIn(rawStat._1(1))
    val hp = (rawStat._1.head.toInt, hpMatch.get.group(1).toInt)
    val atkMatch = statReg.findFirstMatchIn(rawStat._2(1))
    val atk =(rawStat._2.head.toInt, atkMatch.get.group(1).toInt)
    val revMatch = statReg.findFirstMatchIn(rawStat._3(1))
    val rev = (rawStat._3.head.toInt, revMatch.get.group(1).toInt)

    val rawExtraStat = getExtraStat(doc)
    val cost = rawExtraStat._1.head.toInt
    val maxLevel = rawExtraStat._2.head.split(">").last.trim.toInt
    val maxExp = rawExtraStat._3.head.replaceAll(",","").toInt
    val awoken =
      if(rawExtraStat._4.isEmpty) Seq.empty[AwokenSkill]
      else rawExtraStat._4.map{Monster.toAwokenSkill}
    val cawoken =
      if(rawExtraStat._5.isEmpty) Seq.empty[AwokenSkill]
      else rawExtraStat._5.map{Monster.toAwokenSkill}

    val rawRanking = getRanking(doc)
    val ranking = rawRanking

    val (rawASkill,rawLSkill) = getSkills(doc)
    val aSkilldesc = rawASkill(3)
    val askill =
      if(aSkilldesc == "없음") None
      else {
        val aSkillName = rawASkill(1)
        val aSkillMatch = """Lv.1 턴: (\d+) \(Lv.(\d+) 턴: \d+\)""".r.findFirstMatchIn(rawASkill(2))
        val (aSkillMaxTurn,aSkillMaxLevel) = (aSkillMatch.get.group(1).toInt,aSkillMatch.get.group(2).toInt)
        Some(ActiveSkill(aSkillName,aSkillMaxTurn,aSkillMaxLevel,aSkilldesc))
      }
    val lskill =
      if(rawLSkill(1) == "없음") None
      else {
        val enDesc = getLSText(id)
        Some(LeaderSkill(rawLSkill(1),rawLSkill(2),enDesc))
      }

    Monster(idx,krName,jpName,thumbURL,picURL,ty,star,element,hp,atk,rev,cost,maxLevel,maxExp,ranking,awoken,cawoken,askill,lskill,volatile = false)
  }
}
