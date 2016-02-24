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
  def getStat(doc: Document) : MonStat = {
    val stats =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          statsC <- detail >?> elementList("div.m-split.m-clear.no-outter-padding")) yield statsC.flatMap{_ >?> texts("span.m-text1")}
    println(stats)

    val skills =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          skillC <- detail >?> elementList("li.list-one")) yield skillC.flatMap{_ >?> texts("span.m-text1")}
    println(skills)

    if(stats.nonEmpty && skills.nonEmpty) {
      MonStat(0,0,0,0,0,skills.get(0).mkString(" "),skills.get(1).mkString(" "))
    } else MonStat(0,0,0,0,0,"not available","not available")


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
