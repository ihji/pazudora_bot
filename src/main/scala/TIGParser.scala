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
    val statContent = doc >> element("#pad-info-monster-detail") >> elementList("div.m-split.m-clear.no-outter-padding")
    val stats = statContent.map{_ >> texts("span.m-text1")}
    println(stats)

    val skillContent = doc >> element("#pad-info-monster-detail") >> elementList("li.list-one")
    val skills = skillContent.map{_ >> texts("span.m-text1")}
    println(skills)

    MonStat(0,0,0,0,0,skills.lift(0).getOrElse(Seq()).mkString(" "),skills.lift(2).getOrElse(Seq()).mkString(" "))
  }
  def getPic(doc: Document) : (String, String) = {
    val url = doc >> element("#pad-info-monster-detail") >> attrs("src")("img")
    val thumbnail = url.lift(0).getOrElse("")
    val full = url.lift(1).getOrElse("")
    (thumbnail, full)
  }
}
