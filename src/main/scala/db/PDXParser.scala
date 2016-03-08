package db

import data.Monster._
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

/**
  * Created by heejong.lee on 3/7/16.
  */
trait PDXParser {
  def getElementsStringFromUS(monId: MonsterID) : String = {
    val id = monId.id
    val browser = new Browser
    val doc = browser.get(s"http://puzzledragonx.com/en/monster.asp?n=$id")
    val elems = for(elem <- doc >?> elements("table.tableprofile"); e <- elem >?> texts("a")) yield e
    if(elems.nonEmpty) {
      val result = elems.get.flatMap{getElement}
      if(result.length == 1 || result.length == 2) {
        result.map{_.toString}.mkString("/")
      } else "속성이 없습니다."
    } else "속성을 가져오지 못했습니다."
  }
  def getLSText(monId: MonsterID) : String = {
    val id = monId.id
    val browser = new Browser
    val doc = browser.get(s"http://puzzledragonx.com/en/monster.asp?n=$id")
    val elems = for(elem <- doc >?> elements("table#tablestat")) yield elem.flatMap{_ >?> texts("td")}
    val result = for(e <- elems; arr <- e.find{_.exists{_.contains("Leader Skill:")}}) yield {
      val idx = arr.toList.indexOf("Leader Skill:")
      if(idx + 3 < arr.length) {
        val lsText = arr(idx + 3)
        println("leader skill in eng: " + lsText)
        lsText
      } else "리더스킬이 없습니다."
    }
    result.getOrElse("리더스킬 정보를 가지고 오지 못했습니다.")
  }
  private def getElement(str: String) : Option[Element] = {
    str match {
      case _ if str.contains("Fire") => Some(Fire)
      case _ if str.contains("Water") => Some(Water)
      case _ if str.contains("Wood") => Some(Wood)
      case _ if str.contains("Light") => Some(Light)
      case _ if str.contains("Dark") => Some(Dark)
      case _ => None
    }
  }
}
