import data.Monster
import db._
import db.web.{PDXParser, OmatomeruParser}
import parser.{EnermyParser, UserInputParser, TeamParser}
import sem.{EnermySimulator, DamageSimulator}

import scala.io.Source

object PazudoraBot extends TelegramBot(
  Option(System.getenv("PAZUDORA_BOT_KEY")).getOrElse(Source.fromFile("./KEY","UTF-8").getLines.next())
) with OmatomeruParser with PDXParser {
  val db = MonsterDB
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
  on("stat") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"${mon.getNameString}\n${mon.getStatString}")
    }
  }
  on("rank") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"${mon.getNameString}\n${mon.getRanking}")
    }
  }
  on("info") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"${mon.getNameString}\n\n${mon.getInfoString}")
    }
  }
  on("pic") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"[${mon.getNameString}](${mon.picURL})")
    }
  }
  on("show") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"[${mon.getNameString}](${mon.picURL})\n${mon.getStatString}")
    }
  }
  on("roll") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      getGacha()
    }
  }
  on("calc") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      val input = args.mkString(" ")
      val splitted = input.split("=|>").map{_.trim}
      if(splitted.length != 2 && splitted.length != 3) "잘못된 문법입니다."
      else {
        TeamParser.parseTeam(splitted(0)) match {
          case Left(msg) => msg
          case Right(team) =>
            UserInputParser.parse(splitted(1)) match {
              case Left(msg) => msg
              case Right(inp) =>
                println(team + "\n" + inp)
                if(splitted.length == 2) {
                  val sim = new DamageSimulator(team)
                  val damageMap = sim.run(inp)
                  sim.getDamageString(damageMap)
                } else {
                  EnermyParser.parse(splitted(2)) match {
                    case Left(msg) => msg
                    case Right(enermy) =>
                      val sim = new DamageSimulator(team)
                      val damageMap = sim.run(inp)
                      val actualDamageMap = new EnermySimulator(team,damageMap).getActualDamageMap(enermy)
                      sim.getDamageString(actualDamageMap)
                  }
                }
            }
        }
      }
    }
  }
  on("ls") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, mon => s"${mon.getNameString}\n${mon.lSkill.map{x => s"*리더스킬 한글*: ${x.krDesc}\n*리더스킬 영어*: ${x.enDesc}\n*리더스킬 해석*: ${x.toString}"}.getOrElse("리더스킬 없음.")}")
    }
  }
  on("debug") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      val usedMemory = Math.ceil((Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()) / 1024.0 / 1024.0) + "MB"
      val cachedSize = MonsterDB.getDBSize
      s"현재 메모리 사용량: $usedMemory\n현재 캐쉬된 몬스터수: $cachedSize"
    }
  }
  on("version") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      import buildinfo.BuildInfo
      "name: %s\nversion: %s\nscalaVersion: %s\nbuildTime: %s" format (
        BuildInfo.name, BuildInfo.version, BuildInfo.scalaVersion, BuildInfo.builtAtString
        )
    }
  }
}
