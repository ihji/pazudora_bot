import com.pengrad.telegrambot.TelegramBotAdapter
import com.pengrad.telegrambot.model.request.ParseMode

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._

/**
  * Created by ihji on 3/3/16.
  */
class TelegramBot(key: String) {
  val bot = TelegramBotAdapter.build(key)
  var actions : Map[String,(String,Seq[String]) => Future[Unit]] = Map.empty
  var updateOffset : Int = 0
  def on(cmd: String)(handler: (String,Seq[String]) => Future[Unit]): Unit = {
    actions += cmd -> handler
  }
  def replyTo(chatId: String, parseMode: Option[String] = None)(msg: => String): Future[Unit] = {
    val pm = parseMode match {
      case Some("Markdown") => ParseMode.Markdown
      case Some("HTML") => ParseMode.HTML
      case _ => null
    }
    Future(bot.sendMessage(chatId, msg, pm, null, null, null))
  }
  def run(): Unit = {
    while(true) {
      val updates = bot.getUpdates(updateOffset, null, null).updates().asScala
      for(update <- updates) {
        val text = update.message().text()
        val param = makeParam(text)
        if(param.nonEmpty) {
          val chatId = update.message().chat().id()
          val (cmd, args) = param.get
          actions.get(cmd) match {
            case Some(handler) => handler(chatId.toString, args)
            case None =>
          }
        }
        val updateId = update.updateId()
        updateOffset = updateOffset max updateId + 1
      }
      Thread.sleep(1000)
    }
  }
  private def makeParam(text: String) : Option[(String, Seq[String])] = {
    if(text.startsWith("/")) {
      val texts = text.split(' ').map{_.trim}
      Some((texts.head.drop(1),texts.tail.toSeq))
    } else None
  }
}
