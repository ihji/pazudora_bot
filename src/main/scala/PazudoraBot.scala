import info.mukel.telegram.bots.{TelegramBot, Utils, Polling, Commands}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object PazudoraBot extends TelegramBot(
  Option(System.getenv("PAZUDORA_BOT_KEY")).getOrElse(Utils.tokenFromFile("./KEY"))
) with Polling with Commands with TIGParser {
  on("stat") { (sender, args) => Future {
    replyTo(sender) {
      val id = args.headOption
      if(id.nonEmpty) {
        try {
          val doc = getDocument(id.get.toInt)
          getName(doc) + "\n" + getStat(doc)
        } catch {
          case e: NumberFormatException =>
            s"unknown parameter: $args"
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
        try {
          val doc = getDocument(id.get.toInt)
          getName(doc) + "\n" + getPic(doc)._2
        } catch {
          case e: NumberFormatException =>
            s"unknown parameter: $args"
        }
      } else {
        "give me a parameter"
      }
    }
  }}
}
