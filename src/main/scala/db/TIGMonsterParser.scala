package db

import data.Monster
import data.Monster.AwokenSkill

/**
  * Created by ihji on 3/7/16.
  */
trait TIGMonsterParser extends TIGParser with PDXParser {
  val info =
    """No.(\d+) [\p{L}\p{Z},.\(\)\d\+\*・·＝=%]+ ([\p{L}/]+) ★\d+ ([\p{L}/]+)
      |
      |\*HP\* \d+ > (\d+) \([\+-]\d+\)
      |\*공격\* \d+ > (\d+) \([\+-]\d+\)
      |\*회복\* \d+ > (\d+) \([\+-]\d+\)
      |\*코스트\* \d+ \*최대레벨\* \d+ \*총경험치\* [\d,]+ \*환산치\* \d+
      |\*각성스킬\* ([\p{L}\d,\p{Z}]+)
      |
      |\*스킬\* [\p{L}\p{Z},.\(\)\d\+\*・·＝=%]+(Lv.1 턴: \d+ \(Lv.\d+ 턴: \d+\))?
      |[\p{L}\p{Z},.\(\)\d\+\*・·＝=%]+
      |\*리더스킬\* [\p{L}\p{Z},.\(\)\d\+\*・·＝=%]+
      |[\p{L}\p{Z}\n,.\(\)\d\+\*・·＝=%]+""".stripMargin.r
  def getMonster(monId: MonsterID) : Monster = {
    val doc = getDocument(monId)
    val desc = s"${getName(doc)} ${getElementsStringFromUS(monId)}\n\n${getFullStat(doc)}"
    desc match {
      case info(id, ty, elem, hp, atk, rev, awk, _) =>
        println(id, ty, elem, hp, atk, rev, awk)
        val element = {
          val es = elem.split("/")
          es.length match {
            case 2 =>
              (Monster.toElem(es(0)), Some(Monster.toElem(es(1))))
            case 1 =>
              (Monster.toElem(es(0)), None)
          }
        }
        val t = {
          val ts = ty.split("/")
          ts.length match {
            case 3 =>
              (Monster.toType(ts(0).dropRight(2)), Some(Monster.toType(ts(1).dropRight(2))), Some(Monster.toType(ts(2).dropRight(2))))
            case 2 =>
              (Monster.toType(ts(0).dropRight(2)), Some(Monster.toType(ts(1).dropRight(2))), None)
            case 1 =>
              (Monster.toType(ts(0).dropRight(2)), None, None)
          }
        }
        val awoken =
          if(awk.length == 0) Seq.empty[AwokenSkill]
          else {
            val as = awk.split(",").toSeq
            as.map{Monster.toAwokenSkill}
          }
        Monster(id = id.toInt, element = element, ty = t, atk = atk.toInt, hp = hp.toInt, rev = rev.toInt, awokenSkill = awoken)
    }
  }
}
