import info.mukel.telegram.bots.{TelegramBot, Utils, Polling, Commands}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object PazudoraBot extends TelegramBot(Utils.tokenFromFile("./KEY")) with Polling with Commands with TIGParser {
  on("stat") { (sender, args) => Future {
    replyTo(sender) {
      val id = args(0)
      try {
        val doc = getDocument(id.toInt)
        getStat(doc).toString
      } catch {
        case e: NumberFormatException =>
          s"unknown parameter: $args"
      }
    }
  }}
  on("pic") { (sender, args) =>
    replyTo(sender) {
      val id = args(0)
      val doc = getDocument(id.toInt)
      getPic(doc)._2
    }
  }
}
