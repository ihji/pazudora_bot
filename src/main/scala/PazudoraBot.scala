import info.mukel.telegram.bots.{TelegramBot, Utils, Polling, Commands}
import org.jsoup.nodes.Document

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object PazudoraBot extends TelegramBot(
  Option(System.getenv("PAZUDORA_BOT_KEY")).getOrElse(Utils.tokenFromFile("./KEY"))
) with Polling with Commands with TIGParser with OmatomeruParser {
  def nameOrId(input: String) : Either[String,Int] = {
    try {
      val idx = input.toInt
      Right(idx)
    } catch {
      case e : NumberFormatException =>
        val seq = getList(input)
        if(seq.isEmpty) {
          Left("결과가 없습니다.")
        } else if(seq.length == 1) {
          Right(seq.head._1)
        } else {
          if(seq.length > 70) {
            Left(seq.map{_._2}.take(70).mkString("\n") + s"\n\n${seq.length - 70}개의 결과가 생략됨...")
          } else {
            Left(seq.map{_._2}.mkString("\n"))
          }
        }
    }
  }
  def output(args: Seq[String], format: Document => String) : String = {
    if(args.nonEmpty) {
      nameOrId(args.mkString(" ")) match {
        case Left(str) => str
        case Right(idx) =>
          val doc = getDocument(idx)
          format(doc)
      }
    } else {
      "몬스터 ID 나 이름을 입력해주세요."
    }
  }
  on("stat") { (sender, args) => Future {
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, doc => getName(doc) + "\n" + getStat(doc))
    }
  }}
  on("rank") { (sender, args) => Future {
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, doc => getName(doc) + "\n" + getRanking(doc))
    }
  }}
  on("info") { (sender, args) => Future {
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, doc => getName(doc) + "\n\n" + getFullStat(doc))
    }
  }}
  on("pic") { (sender, args) => Future {
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, doc => s"[${getName(doc)}](${getPic(doc)._2})")
    }
  }}
  on("show") { (sender, args) => Future {
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, doc => s"[${getName(doc)}](${getPic(doc)._2})\n${getStat(doc)}")
    }
  }}
  on("roll") { (sender, args) => Future {
    replyTo(sender, parseMode = Some("Markdown")) {
      getGacha()
    }
  }}
}