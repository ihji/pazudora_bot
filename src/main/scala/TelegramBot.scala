import com.pengrad.telegrambot.TelegramBotAdapter
import com.pengrad.telegrambot.model.request.ParseMode

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._
import scala.util.Try

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
    val ret = Future(bot.sendMessage(chatId, msg, pm, null, null, null)).map{_ =>}
    ret.onFailure { case e : Throwable => e.printStackTrace() }
    ret
  }
  def run(): Unit = {
    while(true) {
      val updates = Try(bot.getUpdates(updateOffset, null, null).updates().asScala).getOrElse{
        println("error from getUpdates(), returning empty list"); Seq()
      }
      for(update <- updates) {
        val text = Option(update.message().text()).getOrElse("")
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
