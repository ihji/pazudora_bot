package db

import data.Monster._
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._


/**
  * Created by ihji on 3/6/16.
  */
trait AppBankParser {
  def getElementsStringFromJP(monId: MonsterID) = {
    val element = getElements(monId)
    println("element: "+element)
    if(element.isEmpty) "속성을 가져오지 못했습니다."
    else element.get._1.toString + element.get._2.map{x => "/" + x.toString}.getOrElse("")
  }
  private def getElements(monId: MonsterID) : Option[(Element, Option[Element])] = {
    val id = monId.id
    val browser = new Browser
    val idx = id match {
      case _ if id < 10 => s"00$id"
      case _ if id < 100 => s"0$id"
      case _ => id
    }
    val doc = browser.get(s"http://pd.appbank.net/m$idx.php")
    val elements = for(elem <- doc >?> element("p.icon-attr"); e <- elem >?> elementList("i")) yield e
    elements.flatMap{ list =>
      list.length match {
        case 2 =>
          val firstElem = list(0).toString
          val secondElem = list(1).toString
          Some(getElement(firstElem),Some(getElement(secondElem)))
        case 1 =>
          val firstElem = list.head.toString
          Some(getElement(firstElem),None)
        case _ => None
      }
    }
  }
  private def getElement(str: String) : Element = {
    str match {
      case _ if str.contains("fire") => Fire
      case _ if str.contains("water") => Water
      case _ if str.contains("wood") => Wood
      case _ if str.contains("light") => Light
      case _ if str.contains("dark") => Dark
    }
  }
}
