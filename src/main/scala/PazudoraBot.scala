import info.mukel.telegram.bots.{TelegramBot, Utils, Polling, Commands}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object PazudoraBot extends TelegramBot(
  Option(System.getenv("PAZUDORA_BOT_KEY")).getOrElse(Utils.tokenFromFile("./KEY"))
) with Polling with Commands with TIGParser {
  def nameOrId(input: String) : Either[String,Int] = {
    try {
      val idx = input.toInt
      Right(idx)
    } catch {
      case e : NumberFormatException =>
        val str = getList(input)
        Left(str)
    }
  }
  on("stat") { (sender, args) => Future {
    replyTo(sender) {
      val id = args.headOption
      if(id.nonEmpty) {
        nameOrId(id.get) match {
          case Left(str) => str
          case Right(idx) =>
            val doc = getDocument(idx)
            getName(doc) + "\n" + getStat(doc)
        }
      } else {
        "give me a parameter"
      }
    }
  }}
  on("pic") { (sender, args) => Future {
    replyTo(sender) {
      val id = args.headOption
      if(id.nonEmpty) {
        nameOrId(id.get) match {
          case Left(str) => str
          case Right(idx) =>
            val doc = getDocument(idx)
            getName(doc) + "\n" + getPic(doc)._2
        }

      } else {
        "give me a parameter"
      }
    }
  }}
  on("show") { (sender, args) => Future {
    replyTo(sender) {
      val id = args.headOption
      if(id.nonEmpty) {
        nameOrId(id.get) match {
          case Left(str) => str
          case Right(idx) =>
            val doc = getDocument(idx)
            getName(doc) + "\n" + getPic(doc)._2 + "\n" + getStat(doc)
        }
      } else {
        "give me a parameter"
      }
    }
  }}
}
