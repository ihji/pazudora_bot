import net.ruippeixotog.scalascraper.browser.Browser
import org.jsoup.nodes.Document
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

/**
  * Created by heejong.lee on 2/23/16.
  */
trait TIGParser {
  def getDocument(id: Int) : Document = {
    val browser = new Browser
    val doc = browser.get(s"http://m.thisisgame.com/pad/info/monster/detail.php?code=$id")
    doc
  }
  def getList(name: String) : Seq[(String,Int)] = {
    ???
  }
  def getName(doc: Document) : String = {
    val names =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          nameC  <- detail >?> elementList("div.tag-a.no-margin")) yield nameC.flatMap{_ >?> texts("span.m-text1")}
    if(names.nonEmpty) {
      names.get(0).mkString(" ")
    } else "not available"
  }
  def getStat(doc: Document) : String = {
    val stats =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          statsC <- detail >?> elementList("div.m-split.m-clear.no-outter-padding")) yield statsC.flatMap{_ >?> texts("span.m-text1")}
    println(stats)

    val skills =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          skillC <- detail >?> elementList("li.list-one")) yield skillC.flatMap{_ >?> texts("span.m-text1")}
    println(skills)

    if(stats.nonEmpty && skills.nonEmpty) {
      s"""${stats.get(0).mkString(" ")}
         |${stats.get(1).mkString(" ")}
         |${stats.get(2).mkString(" ")}
         |${skills.get(0).mkString(" ")}
         |${skills.get(1).mkString(" ")}
       """.stripMargin
    } else "not available"


  }
  def getPic(doc: Document) : (String, String) = {
    val urls =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          url    <- detail >?> attrs("src")("img")) yield url

    if(urls.nonEmpty) {
      (urls.get(0), urls.get(1))
    } else ("not available", "not available")
  }
}
