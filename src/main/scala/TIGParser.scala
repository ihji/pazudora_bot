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
  def getList(name: String) : Seq[(Int, String)] = {
    val browser = new Browser
    val doc = browser.get(s"http://m.thisisgame.com/pad/info/monster/list.php?sf=name&sw=$name")
    val targets = for(list <- doc >?> elementList("li.list-one.split-one.half-padding")) yield list.flatMap{_ >?> texts("span.m-text1")}

    println(targets)

    if(targets.nonEmpty) {
      targets.get.map{x => (x(0).drop(3).toInt,x.updated(1,s"*${x(1)}*").mkString(" "))}
    } else Seq.empty
  }
  def getName(doc: Document) : String = {
    val names =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          nameC  <- detail >?> elementList("div.tag-a.no-margin")) yield nameC.flatMap{_ >?> texts("span.m-text1")}
    println("name: " + names)

    if(names.nonEmpty) {
      val result = names.get(0)
      result.updated(1,s"*${result(1)}*").mkString(" ")
    } else "이름을 가져오지 못했습니다."
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
  def getFullStat(doc: Document) : String = {
    val stats =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          statsC <- detail >?> elementList("div.m-split.m-clear.no-outter-padding")) yield {
        statsC.flatMap{_ >?> texts("span.m-text1")} ++ statsC.flatMap{_ >?> attrs("alt")("img.m-image1.size-h1p75")}.filter{_.nonEmpty}
      }
    println("stats: " + stats)

    val skills =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          skillC <- detail >?> elementList("li.list-one")) yield skillC.flatMap{_ >?> texts("span.m-text1")}
    println("skills: " + skills)

    val totals =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          totalC <- detail >?> elementList("div.m-split.split-4.no-outter-padding")) yield totalC.flatMap{_ >?> texts("span.m-text1")}
    println("totals: " + totals)

    if(stats.nonEmpty && skills.nonEmpty && totals.nonEmpty) {
      val hp = stats.get(0)
      val atk = stats.get(1)
      val rev = stats.get(2)
      val cost = stats.get(3)
      val maxlevel = stats.get(4)
      val maxexp = stats.get(5)
      val awks = stats.get(6) ++ stats.get.last

      val totalPtr = totals.get(0)(3).split(":").map{_.trim}

      val askill = skills.get(0)
      val lskill = skills.get(1)

      s"""${hp.updated(0,s"*${hp(0)}*").mkString(" ")}
         |${atk.updated(0,s"*${atk(0)}*").mkString(" ")}
         |${rev.updated(0,s"*${rev(0)}*").mkString(" ")}
         |${cost.updated(0,s"*${cost(0)}*").mkString(" ")} ${maxlevel.updated(0,s"*${maxlevel(0)}*").mkString(" ")} ${maxexp.updated(0,s"*${maxexp(0)}*").mkString(" ")} ${totalPtr.updated(0,s"*${totalPtr(0)}*").mkString(" ")}
         |*${awks(0)}* ${awks.tail.mkString(", ")}
         |
         |${askill.take(3).updated(0,s"*${askill(0)}*").mkString(" ")}
         |${askill.drop(3).mkString(" ")}
         |${lskill.take(2).updated(0,s"*${lskill(0)}*").mkString(" ")}
         |${lskill.drop(2).mkString(" ")}
       """.stripMargin
    } else "상세정보를 가져오지 못했습니다."
  }
  def getStat(doc: Document) : String = {
    val stats =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          statsC <- detail >?> elementList("div.m-split.m-clear.no-outter-padding")) yield statsC.flatMap{_ >?> texts("span.m-text1")}
    println("stats: " + stats)

    val skills =
      for(detail <- doc >?> element("#pad-info-monster-detail");
          skillC <- detail >?> elementList("li.list-one")) yield skillC.flatMap{_ >?> texts("span.m-text1")}
    println("skills: " + skills)

    if(stats.nonEmpty && skills.nonEmpty) {
      val hp = stats.get(0)
      val atk = stats.get(1)
      val rev = stats.get(2)

      val askill = skills.get(0)
      val lskill = skills.get(1)

      s"""${hp.updated(0,s"*${hp(0)}*").mkString(" ")}
         |${atk.updated(0,s"*${atk(0)}*").mkString(" ")}
         |${rev.updated(0,s"*${rev(0)}*").mkString(" ")}
         |${askill.take(3).updated(0,s"*${askill(0)}*").mkString(" ")}
         |${askill.drop(3).mkString(" ")}
         |${lskill.take(2).updated(0,s"*${lskill(0)}*").mkString(" ")}
         |${lskill.drop(2).mkString(" ")}
       """.stripMargin
    } else "정보를 가져오지 못했습니다."
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
