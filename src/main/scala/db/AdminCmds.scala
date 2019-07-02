package db

import java.math.RoundingMode
import java.text.DecimalFormat

import data.RareEggs

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Created by heejong.lee on 3/23/16.
  */

trait AdminCmds {
  var eggs : RareEggs

  val adminKey = Option(System.getenv("PAZUDORA_BOT_ADMIN_KEY")).getOrElse(Source.fromFile("./ADMIN_KEY","UTF-8").getLines.next())
  def handleAdminCmds(args: Seq[String]) = {
    if(args.headOption.contains(adminKey)) {
      val cmd = if(args.length >= 2) args(1) else ""
      val param = if(args.length >= 3) args.drop(2).mkString(" ") else ""
      cmd match {
        case "pset" =>
          setEggFromPreset(eggs, param) match {
            case Left(x) => x
            case Right(e) =>
              eggs = e
              "레어에그 대상 몬스터가 수정되었습니다."
          }
        case "rset" => rollSetting(eggs)
        case "clcc" => clearCache(param)
        case _ =>
          """커맨드는 다음 중 하나여야 합니다:
            |pset : 레어에그 대상 몬스터를 프리셋에서 선택합니다.
            |rset : 현재 레어에그 셋팅을 출력합니다.
            |clcc : 몬스터 캐쉬를 제거합니다.
          """.stripMargin
      }
    } else "어드민 키가 맞지 않습니다."
  }
  private def setEggFromPreset(egg: RareEggs, args: String) : Either[String,RareEggs] = {
    val newProb = RareEggs.getProb(args)
    if(newProb.isEmpty) Left(
      s"${args}를 찾을 수 없습니다. 가능한 프리셋 중 하나를 선택해주세요:\n${
        RareEggs.probData.keys.map{_.toString}.toList.sorted.mkString("\n")
      }")
    else Right(egg.copy(prob = newProb))
  }
  private def rollSetting(egg: RareEggs) : String = {
    val df = new DecimalFormat("#.##")
    df.setRoundingMode(RoundingMode.HALF_UP);
    s"""roll 셋팅
       |
       |레어에그 대상몬스터:
       |${egg.prob.toList.sortBy{_._1}.map{case (k,v) => df.format(k)+"%: "+v.mkString(", ")}.mkString("\n")}
       |
       |총 확률: ${df.format(egg.targets.length.toDouble / 10.0)}%
     """.stripMargin
  }

  private def clearCache(args: String) : String = {
    if(args == "all") {
      MonsterDB.clearMonsterCache(None)
      "전체 몬스터 캐쉬를 삭제합니다."
    } else Try(args.toInt) match {
      case Success(x) =>
        MonsterDB.clearMonsterCache(Some(x))
        s"몬스터 $x 에 대한 캐쉬를 삭제합니다."
      case Failure(_) => "몬스터 아이디 또는 'all' 을 입력해주세요."
    }
  }
}
