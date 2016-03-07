import db.{OmatomeruParser, AppBankParser}
import org.jsoup.nodes.Document

import scala.io.Source

object PazudoraBot extends TelegramBot(
  Option(System.getenv("PAZUDORA_BOT_KEY")).getOrElse(Source.fromFile("./KEY","UTF-8").getLines.next())
) with TIGParser with OmatomeruParser with AppBankParser {
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
          if(seq.length > 60) {
            Left(seq.map{_._2}.takeRight(60).mkString("\n") + s"\n\n오래된 ${seq.length - 60}개의 결과가 생략됨...")
          } else {
            Left(seq.map{_._2}.mkString("\n"))
          }
        }
    }
  }
  def output(args: Seq[String], format: (Int,Document) => String) : String = {
    if(args.nonEmpty) {
      nameOrId(args.mkString(" ")) match {
        case Left(str) => str
        case Right(idx) =>
          val doc = getDocument(idx)
          format(idx,doc)
      }
    } else {
      "몬스터 ID 나 이름을 입력해주세요."
    }
  }
  on("stat") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, (idx,doc) => s"${getName(doc)} ${getElementsString(idx)}\n${getStat(doc)}")
    }
  }
  on("rank") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, (idx,doc) => s"${getName(doc)} ${getElementsString(idx)}\n${getRanking(doc)}")
    }
  }
  on("info") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, (idx,doc) => s"${getName(doc)} ${getElementsString(idx)}\n\n${getFullStat(doc)}")
    }
  }
  on("pic") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, (idx,doc) => s"[${getName(doc)} ${getElementsString(idx)}](${getPic(doc)._2})")
    }
  }
  on("show") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      output(args, (idx,doc) => s"[${getName(doc)} ${getElementsString(idx)}](${getPic(doc)._2})\n${getStat(doc)}")
    }
  }
  on("roll") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      getGacha()
    }
  }
  on("calc") { (sender, args) =>
    replyTo(sender, parseMode = Some("Markdown")) {
      "not implemented."
    }
  }
}
