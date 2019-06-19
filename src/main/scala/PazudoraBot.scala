import data.{Monster, RareEggs}
import db._
import db.web.PDXParser
import parser.{EnermyParser, TeamParser, UserInputParser}
import sem.{DamageSimulator, EnermySimulator}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source

object PazudoraBot extends TelegramBot(
  Option(System.getenv("PAZUDORA_BOT_KEY")).getOrElse(Source.fromFile("./KEY","UTF-8").getLines.next())
) with PDXParser with AdminCmds {
  val db = MonsterDB
  var eggs = RareEggs(Some(Monster.Fire),None,Set())
  def output(args: Seq[String], format: Monster => String) : String = {
    if(args.nonEmpty) {
      db.searchMonster(args.mkString(" ")) match {
        case Left(str) => str
        case Right(mon) => format(mon)
      }
    } else {
      "몬스터 ID 나 이름을 입력해주세요."
    }
  }
  on("stat", "몬스터의 스테이터스를 출력합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"${mon.getNameString}\n${mon.getStatString}")
    }
  }
  on("rank", "몬스터의 랭크를 출력합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"${mon.getNameString}\n${mon.getRanking}")
    }
  }
  on("info", "몬스터의 상세 정보를 츨력합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"${mon.getNameString}\n\n${mon.getInfoString}")
    }
  }
  on("pic", "몬스터의 일러스트를 출력합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"[${mon.getNameString}](${mon.picURL})")
    }
  }
  on("show", "몬스터의 일러스트와 스테이터스를 출력합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"[${mon.getNameString}](${mon.picURL})\n${mon.getStatString}")
    }
  }
  on("roll", "랜덤하게 몬스터를 하나 뽑습니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      val id = eggs.pick()
      db.searchMonster(id.toString) match {
        case Left(str) => str
        case Right(mon) =>
          s"[${mon.getNameString}](${mon.picURL})"
      }
    }
  }
  on("calc", "예상 데미지를 계산합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      val input = args.mkString(" ")
      val splitted = input.split("=|>").map{_.trim}
      if(splitted.length != 2 && splitted.length != 3) "잘못된 문법입니다."
      else {
        TeamParser.parseTeam(splitted(0)) match {
          case Left(msg) => msg
          case Right(team) =>
            UserInputParser.parseInput(splitted(1)) match {
              case Left(msg) => msg
              case Right(inp) =>
                println(team + "\n" + inp)
                val sim = new DamageSimulator(team)
                val totalHp = sim.calculateHP
                val totalRev = sim.calculateRev
                val damageMap = sim.calculateDamage(inp)
                if(splitted.length == 2) {
                  s"*팀 HP* $totalHp *팀 회복* $totalRev\n\n" + sim.getDamageString(damageMap)
                } else {
                  EnermyParser.parseInput(splitted(2)) match {
                    case Left(msg) => msg
                    case Right(enermy) =>
                      val actualDamageMap = new EnermySimulator(team,damageMap).getActualDamageMap(enermy)
                      s"*팀 HP*: $totalHp *팀 회복* $totalRev\n\n" + sim.getDamageString(actualDamageMap)
                  }
                }
            }
        }
      }
    }
  }
  on("ls", "리더스킬을 출력합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"${mon.getNameString}\n${mon.lSkill.map{x => s"*리더스킬 한글*: ${x.krDesc}\n*리더스킬 영어*: ${x.enDesc}\n*리더스킬 해석*: ${x.toString}"}.getOrElse("리더스킬 없음.")}")
    }
  }
  on("debug", "디버그 정보를 출력합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      val usedMemory = Math.ceil((Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()) / 1024.0 / 1024.0) + "MB"
      val cachedSize = Await.result(db.getDBSize, Duration(3, "seconds"))
      s"현재 메모리 사용량: $usedMemory\n현재 캐쉬된 몬스터수: $cachedSize"
    }
  }
  on("version", "버전 정보를 출력합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      import buildinfo.BuildInfo
      "name: %s\nversion: %s\nscalaVersion: %s\nbuildTime: %s" format (
        BuildInfo.name, BuildInfo.version, BuildInfo.scalaVersion, BuildInfo.builtAtString
        )
    }
  }
  on("admin", "레어에그 및 내부구조를 관리합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      handleAdminCmds(args)
    }
  }
  on("help", "도움말을 출력합니다.") { (sender, user, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      "파즈도라 텔레그램 봇 도움말.\n\n" + actions.toSeq.sortBy(_._1).map{case (cmd,(desc,_)) => s"*/$cmd* : $desc"}.mkString("\n")
    }
  }
}
