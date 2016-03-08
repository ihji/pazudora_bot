import db.{MonsterID, TIGParser, OmatomeruParser, AppBankParser}
import org.jsoup.nodes.Document
import parser.{DamageConditionParser, TeamParser}

import scala.io.Source

object PazudoraBot extends TelegramBot(
  Option(System.getenv("PAZUDORA_BOT_KEY")).getOrElse(Source.fromFile("./KEY","UTF-8").getLines.next())
) with TIGParser with OmatomeruParser with AppBankParser {
  def output(args: Seq[String], format: (MonsterID,Document) => String) : String = {
    if(args.nonEmpty) {
      nameOrId(args.mkString(" ")) match {
        case Left(str) => str
        case Right(id) =>
          val doc = getDocument(id)
          format(id,doc)
      }
    } else {
      "몬스터 ID 나 이름을 입력해주세요."
    }
  }
  on("stat") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, (id,doc) => s"${getName(doc)} ${getElementsString(id)}\n${getStat(doc)}")
    }
  }
  on("rank") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, (id,doc) => s"${getName(doc)} ${getElementsString(id)}\n${getRanking(doc)}")
    }
  }
  on("info") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, (id,doc) => s"${getName(doc)} ${getElementsString(id)}\n\n${getFullStat(doc)}")
    }
  }
  on("pic") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, (id,doc) => s"[${getName(doc)} ${getElementsString(id)}](${getPic(doc)._2})")
    }
  }
  on("show") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, (id,doc) => s"[${getName(doc)} ${getElementsString(id)}](${getPic(doc)._2})\n${getStat(doc)}")
    }
  }
  on("roll") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      getGacha()
    }
  }
  on("calc") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
//      val input = args.mkString(" ")
//      val splitted = input.split("=").map{_.trim}
//      if(splitted.length != 2) "잘못된 문법입니다."
//      else {
//        val debug = TeamParser.parseTeam(splitted(0)) match {
//          case Left(msg) => msg
//          case Right(team) => team.toString
//        }
//        val debug2 = DamageConditionParser.parse(splitted(1))
//        debug + "///" + debug2
//      }
      val x = new db.PDXLeaderSkillParser{}.getLeaderSkill(MonsterID(args.head.toInt,args.head.toInt))
      x.toString
    }
  }
}
