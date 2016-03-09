package db

import net.ruippeixotog.scalascraper.browser.Browser
import org.jsoup.nodes.Document
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

/**
  * Created by heejong.lee on 2/23/16.
  */
trait TIGParser {
  def getDocument(monId: MonsterID) : Document = {
    val id = monId.TIGid
    val browser = new Browser
    val doc = browser.get(s"http://m.thisisgame.com/pad/info/monster/detail.php?code=$id")
    doc
  }
  def getName(doc: Document) : Seq[String] = {
    val names =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          nameC  <- detail >?> elementList("div.tag-a.no-margin")) yield nameC.flatMap{_ >?> texts("span.m-text1")}
    println("name: " + names)

    if(names.nonEmpty) {
      val result = names.get(0)
      result
    } else Seq.empty
  }
  def getRanking(doc: Document) : String = {
    val totals =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          totalC <- detail >?> elementList("div.m-split.split-4.no-outter-padding")) yield totalC.flatMap{_ >?> texts("span.m-text1")}
    println("totals: " + totals)

    val rankings =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          rankingC <- detail >?> elementList("div.m-table2")) yield {
        rankingC.flatMap{_ >?> attrs("alt")("img.m-image1.size-w1p25")} ++ rankingC.flatMap{_ >?> texts("span.m-text1")}
      }
    println("rankings: " + rankings)

    if(totals.nonEmpty && rankings.nonEmpty) {
      val titles = "전체" +: rankings.get(0).toList
      val positions = rankings.get(1).drop(2).sliding(titles.length+1,titles.length+1).toList

      val titleWithPositions =
        (for((title,idx) <- titles.zipWithIndex; position <- positions) yield {
          s"$title ${position(0)}: ${position(idx + 1)}"
        }).sliding(4,4).toList

      s"""${totals.get(0).mkString(", ")}
         |${titleWithPositions.map{_.mkString(", ")}.mkString("\n")}
       """.stripMargin
    } else "랭킹정보를 가져오지 못했습니다."
  }
  def getExtraStat(doc: Document) : (Seq[String],Seq[String],Seq[String],Seq[String],Seq[String]) = {
    val stats =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          statsC <- detail >?> elementList("div.m-split.m-clear.no-outter-padding")) yield {
        statsC.flatMap{_ >?> texts("span.m-text1")} ++ statsC.flatMap{_ >?> attrs("alt")("img.m-image1.size-h1p75")}.filter{_.nonEmpty}
      }
    println("stats: " + stats)

    val totals =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          totalC <- detail >?> elementList("div.m-split.split-4.no-outter-padding")) yield totalC.flatMap{_ >?> texts("span.m-text1")}
    println("totals: " + totals)

    if(stats.nonEmpty && totals.nonEmpty) {
      val cost = stats.get(3)
      val maxlevel = stats.get(4)
      val maxexp = stats.get(5)
      val awks = if(stats.get.toString.contains("각성스킬")) stats.get(6) ++ stats.get.last else List("각성스킬", "없음")

      val totalPtr = totals.get(0)(3).split(":").map{_.trim}

      (cost, maxlevel, maxexp, awks, totalPtr)
    } else (Seq.empty,Seq.empty,Seq.empty,Seq.empty,Seq.empty)
  }
  def getStat(doc: Document) : (Seq[String],Seq[String],Seq[String]) = {
    val stats =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          statsC <- detail >?> elementList("div.m-split.m-clear.no-outter-padding")) yield statsC.flatMap{_ >?> texts("span.m-text1")}
    println("stats: " + stats)

    if(stats.nonEmpty) {
      val hp = stats.get(0)
      val atk = stats.get(1)
      val rev = stats.get(2)
      (hp,atk,rev)
    } else (Seq.empty,Seq.empty,Seq.empty)
  }
  def getSkills(doc: Document) : (Seq[String],Seq[String]) = {
    val skills =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          skillC <- detail >?> elementList("li.list-one")) yield skillC.flatMap{_ >?> texts("span.m-text1")}
    println("skills: " + skills)

    if(skills.nonEmpty) {
      val (askill, lskill) =
        (skills.get.toString.contains("(스킬"), skills.get.toString.contains("리더스킬")) match {
          case (true, true) => (skills.get(0), skills.get(1))
          case (true, false) => (skills.get(0), Seq("리더스킬", "", "없음"))
          case (false, true) => (Seq("스킬", "", "", "없음"), skills.get(0))
          case (false, false) => (Seq("스킬", "", "", "없음"), Seq("리더스킬", "", "없음"))
        }

      (askill, lskill)
    } else (Seq.empty,Seq.empty)
  }
  def getPic(doc: Document) : (String, String) = {
    val urls =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          url    <- detail >?> attrs("src")("img")) yield url

    if(urls.nonEmpty) {
      (urls.get(0), urls.get(1))
    } else ("섬네일을 가져오지 못했습니다.", "사진을 가져오지 못했습니다.")
  }
}
