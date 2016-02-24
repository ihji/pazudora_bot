import info.mukel.telegram.bots.{TelegramBot, Utils, Polling, Commands}

object PazudoraBot extends TelegramBot(Utils.tokenFromFile("./KEY")) with Polling with Commands {
  on("hello") { (sender, args) =>
    replyTo(sender) {
      "My token is safe!!!"
    }
  }
}
