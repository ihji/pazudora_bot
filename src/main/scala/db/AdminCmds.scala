package db

import data.{Monster, RareEggs}

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
        /*
        case "engf" =>
          enableGodFest(eggs, param) match {
            case Right(newEggs) =>
              eggs = newEggs
              "갓페가 적용 되었습니다."
            case Left(msg) => msg
          }
        case "digf" =>
          eggs = disableGodFest(eggs)
          "갓페 목록이 삭제 되었습니다."
        case "enca" =>
          enableCarnival(eggs, param) match {
            case Right(newEggs) =>
              eggs = newEggs
              "카니발이 적용 되었습니다."
            case Left(msg) => msg
          }
        case "dica" =>
          eggs = disableCarnival(eggs)
          "카니발이 제거 되었습니다."
        case "gfbe" =>
          setGodFestBenefit(eggs, param) match {
            case Left(x) => x
            case Right(e) =>
              eggs = e
              "갓페 대상신 배율이 수정되었습니다."
          }
        case "glbe" =>
          setGodFestLimitedBenefit(eggs, param) match {
            case Left(x) => x
            case Right(e) =>
              eggs = e
              "갓페 한정신 배율이 수정되었습니다."
          }
        case "cabe" =>
          setCarnivalBenefit(eggs, param) match {
            case Left(x) => x
            case Right(e) =>
              eggs = e
              "카니발 대상 배율이 수정되었습니다."
          }
        case "rset" => rollSetting(eggs)
        */
        case "clcc" => clearCache(param)
        case _ =>
          """커맨드는 다음 중 하나여야 합니다:
            |engf : enable god fest event
            |digf : disable god fest event
            |enca : enable carnival event
            |dica : disable carnival event
            |gfbe : set god fest benefit
            |glbe : set god fest limited benefit
            |cabe : set carnival benefit
            |rset : print roll setting
            |clcc : clear monster cache
          """.stripMargin
      }
    } else "어드민 키가 맞지 않습니다."
  }
  /*
  private def setGodFestBenefit(egg: RareEggs, args: String) : Either[String,RareEggs] = {
    Try(args.toInt) match {
      case Success(x) =>
        Right(egg.copy(GODFEST_BENEFIT = x))
      case Failure(_) => Left("숫자가 아닙니다.")
    }
  }
  private def setGodFestLimitedBenefit(egg: RareEggs, args: String) : Either[String,RareEggs] = {
    Try(args.toInt) match {
      case Success(x) =>
        Right(egg.copy(GODFEST_LIMITED_BENEFIT = x))
      case Failure(_) => Left("숫자가 아닙니다.")
    }
  }
  private def setCarnivalBenefit(egg: RareEggs, args: String) : Either[String,RareEggs] = {
    Try(args.toInt) match {
      case Success(x) =>
        Right(egg.copy(CARNIVAL_BENEFIT = x))
      case Failure(_) => Left("숫자가 아닙니다.")
    }
  }
  private def enableGodFest(egg: RareEggs, args: String) : Either[String,RareEggs] = {
    def applyTargets(e: RareEggs, targets: String, exceptions: Option[String]) : Either[String,RareEggs] = {
      def targetApplied(e: RareEggs) = {
        val godFestList = targets.split(',').map{_.trim}.toList
        if(godFestList.forall{RareEggs.str2GodFestTargets(_).nonEmpty}) {
          Success(e.copy(godFest = Some(godFestList.flatMap{RareEggs.str2GodFestTargets})))
        } else Failure(new Exception(s"갓페 대상신은 다음 중 하나여야 합니다:\n${RareEggs.godFestTargets.mkString("\n")}"))
      }
      def exceptionApplied(e: RareEggs) =
        if(exceptions.nonEmpty) {
          val exceptionSet = Try(exceptions.get.split(',').map{_.trim.toInt}.toSet) match {
            case x@Success(_) => x
            case x@Failure(_) => Failure(new Exception("올바른 숫자가 아닙니다."))
          }
          exceptionSet.map{s => e.copy(excludeGodFestLimited = s)}
        } else Success(e)
      targetApplied(e) flatMap exceptionApplied match {
        case Success(x) => Right(x)
        case Failure(x) => Left(x.getMessage)
      }
    }
    val splitted = args.split("except").map{_.trim}
    splitted.length match {
      case 2 => applyTargets(egg, splitted(0),Some(splitted(1)))
      case 1 => applyTargets(egg, splitted(0), None)
      case _ =>
        Left("syntax: '갓페 대상신 리스트' except '제외된 갓페 한정신 번호 리스트' 또는 '갓페 대상신 리스트'")
    }
  }
  private def disableGodFest(egg: RareEggs) : RareEggs = {
    egg.copy(godFest = None, excludeGodFestLimited = Set())
  }
  private def enableCarnival(egg: RareEggs, args: String) : Either[String,RareEggs] = {
    Try(Monster.toElem(args)) match {
      case Success(e) => Right(egg.copy(carnival = Some(e)))
      case Failure(_) => Left("syntax: '불, 물, 나무, 빛, 어둠' 중 하나")
    }
  }
  private def disableCarnival(egg: RareEggs) : RareEggs = {
    egg.copy(carnival = None)
  }
  private def rollSetting(egg: RareEggs) : String = {
    s"""roll 셋팅
       |
       |카니발 대상 속성: ${egg.carnival.getOrElse("없음.")}
       |갓페 대상신: ${egg.godFest.map{_.mkString(",")}.getOrElse("없음.")}
       |갓페 제외 한정신: ${if(egg.excludeGodFestLimited.nonEmpty) egg.excludeGodFestLimited.toList.sorted.mkString(",") else "없음."}
       |
       |카니발 확률 보정: ${egg.CARNIVAL_BENEFIT} 배
       |갓페 대상신 확률 보정: ${egg.GODFEST_BENEFIT} 배
       |갓페 한정신 확률 보정: ${egg.GODFEST_LIMITED_BENEFIT} 배
     """.stripMargin
  }
  */
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
