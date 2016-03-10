package db

import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

/**
  * Created by heejong.lee on 3/9/16.
  */
trait TIGSearch {
  def nameOrId(rawInput: String) : Either[String,MonsterID] = {
    val (isKROpt,input) =
      if(rawInput.toLowerCase.endsWith("+kr")) (Some(true),rawInput.dropRight(3).trim)
      else if(rawInput.toLowerCase.endsWith("+jp")) (Some(false),rawInput.dropRight(3).trim)
      else (None,rawInput)
    val idOpt = util.control.Exception.catching(classOf[NumberFormatException]) opt input.toInt
    val seq = idOpt match {
      case Some(id) =>
        val idSearch =
          if(isKROpt.getOrElse(false)) s"http://m.thisisgame.com/pad/info/monster/list.php?numkr=$id"
          else s"http://m.thisisgame.com/pad/info/monster/list.php?numjp=$id"
        getListFromURL(idSearch)
      case None =>
        val nameSearch = s"http://m.thisisgame.com/pad/info/monster/list.php?sf=name&sw=$input"
        getListFromURL(nameSearch)
    }
    if(seq.isEmpty) {
      Left("검색 결과가 없습니다.")
    } else if(seq.length == 1) {
      if(idOpt.nonEmpty && idOpt.get != seq.head._1 && isKROpt.isEmpty) {
        Left(
          s"""한국과 일본 아이디가 다른 몬스터입니다. 정확한 검색을 위해 서버를 지정해 주세요. 명령어 맨 마지막에 +kr 또는 +jp 를 추가하면 됩니다.
              |일본 아이디: ${idOpt.get}, 한국 아이디 ${seq.head._1}, 이름: ${seq.head._3}
           """.stripMargin)
      } else {
        Right(MonsterID(seq.head._1,seq.head._2))
      }
    } else {
      if(seq.length > 60) {
        Left("대상 몬스터가 여러개 입니다.\n"+seq.map{_._3}.takeRight(60).mkString("\n") + s"\n\n오래된 ${seq.length - 60}개의 결과가 생략됨...")
      } else {
        Left("대상 몬스터가 여러개 입니다.\n"+seq.map{_._3}.mkString("\n"))
      }
    }
  }
  private def getListFromURL(url: String) : Seq[(Int, Int, String)] = {
    val browser = new Browser
    val doc = browser.get(url)
    val targets = for(list <- doc >?> elementList("li.list-one.split-one.half-padding")) yield
      list.flatMap{x => for(a <- x >?> texts("span.m-text1"); b <- x >?> attr("href")("a")) yield (a,b)}

    println(targets)

    if(targets.nonEmpty) {
      targets.get.map{case (x, y) => (x(0).drop(3).toInt,y.drop(16).toInt,x.updated(1,s"*${x(1)}*").mkString(" "))}
    } else Seq.empty
  }
}
