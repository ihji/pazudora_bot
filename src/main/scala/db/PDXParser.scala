package db

import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

/**
  * Created by heejong.lee on 3/7/16.
  */
trait PDXParser {
  def getLSText(id: Int) : String = {
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
}
