package db

import data.Monster

/**
  * Created by ihji on 3/7/16.
  */
trait TIGMonsterParser {
  val info =
    """No.(\d+) [\p{L}\*・＝= ]+ ([\p{L}/]+) ★\d+ ([\p{L}/]+)
      |
      |\*HP\* \d+ > (\d+) \([\+-]\d+\)
      |\*공격\* \d+ > (\d+) \([\+-]\d+\)
      |\*회복\* \d+ > (\d+) \([\+-]\d+\)
      |\*코스트\* \d+ \*최대레벨\* \d+ \*총경험치\* [\d,]+ \*환산치\* \d+
      |\*각성스킬\* [\p{L}\d, ]+
      |
      |\*스킬\* [\p{L} ]+ Lv.1 턴: \d+ \(Lv.\d+ 턴: \d+\)
      |[\p{L},.\(\)\d\+=  ]+
      |\*리더스킬\* [\p{L} ]+
      |[\p{L},.\(\)\d\+=\n  ]+""".stripMargin.r
  def getMonster(desc: String) : Monster = {
    desc match {
      case info(id,ty,elem,hp,atk,rev) => println(id,ty,elem,hp,atk,rev)
      case _ => println("match error")
    }
    ???
  }
}
