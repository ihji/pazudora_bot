package db.web

import db.MonsterID
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.model.Document

/**
  * Created by heejong.lee on 2/23/16.
  */
trait TIGParser {
  def getDocument(monId: MonsterID) : Document = {
    val id = monId.TIGid
    val browser = new JsoupBrowser
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
      result.toList
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
        (rankingC.flatMap{_ >?> attrs("alt")("img.m-image1.size-w1p25")} ++ rankingC.flatMap{_ >?> texts("span.m-text1")}).map{_.toList}
      }
    println("rankings: " + rankings)

    if(totals.nonEmpty && rankings.nonEmpty) {
      val titles = "전체" +: rankings.get(0)
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
  def getExtraStat(doc: Document) : (Seq[String], Seq[String], Seq[String], Seq[String], Seq[String]) = {
    val stats =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          groups <- detail >?> elementList("div.m-comp-group");
          statsC <- groups(4) >?> texts("span")) yield statsC.toList
    println("extra stats: " + stats)

    val askills =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          groups <- detail >?> elementList("div.m-comp-group");
          imgs <- groups(8) >?> elementList("img")) yield imgs.flatMap{_ >?> attr("alt")}
    println("askills: " + askills)

    val caskills =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          groups <- detail >?> elementList("div.m-comp-group");
          imgs <- groups(10) >?> elementList("img")) yield imgs.flatMap{_ >?> attr("alt")}
    println("caskills: " + caskills)

    val cost = stats.toSeq.map{_(1)}
    val maxlevel = stats.toSeq.map{_(3)}
    val maxexp = stats.toSeq.map{_(5)}
    val awks = askills.toSeq.flatten
    val cawks = caskills.toSeq.flatten

    (cost, maxlevel, maxexp, awks, cawks)
  }
  def getStat(doc: Document) : (Seq[String], Seq[String], Seq[String]) = {
    val stats =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          groups <- detail >?> elementList("div.m-comp-group");
          statsC <- groups(2) >?> texts("span")) yield statsC.toList
    println("stats: " + stats)

    if(stats.nonEmpty) {
      val hp = Seq(stats.get(5), stats.get(9))
      val atk = Seq(stats.get(6), stats.get(10))
      val rev = Seq(stats.get(7), stats.get(11))
      (hp, atk, rev)
    } else (Seq.empty, Seq.empty, Seq.empty)
  }
  def getSkills(doc: Document) : (Seq[String], Seq[String]) = {
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

      (askill.toList, lskill.toList)
    } else (Seq.empty, Seq.empty)
  }
  def getPic(doc: Document) : (String, String) = {
    val urls =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          url    <- detail >?> attrs("src")("img")) yield url

    if(urls.nonEmpty) {
      (urls.get.toList(0), urls.get.toList(1))
    } else ("섬네일을 가져오지 못했습니다.", "사진을 가져오지 못했습니다.")
  }
}
