package db

import data.Monster
import db.web.TIGSearch
import db.web.TIGSearch.NotFound

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Created by heejong.lee on 3/9/16.
  */
object MonsterDB extends MonsterParser with InMemoryCache with TIGSearch {
  def getMonster(id: MonsterID) : Monster = {
    val cachedMon = Await.result(get(id), Duration(3, "seconds"))
    if(cachedMon.isEmpty || cachedMon.get.volatile) {
      val mon = parseMonster(id)
      put(id, mon)
      mon
    } else cachedMon.get
  }
  def searchMonster(rawInput: String) : Either[String,Monster] = {
    val (isKROpt,input) =
      if(rawInput.toLowerCase.endsWith("+kr")) (Some(true),rawInput.dropRight(3).trim)
      else if(rawInput.toLowerCase.endsWith("+jp")) (Some(false),rawInput.dropRight(3).trim)
      else (None,rawInput)
    nameOrId(input,isKROpt) match {
      case Left(NotFound(str)) => //TODO: also need to handle +kr, +jp tags for cache
        val idOpt = util.control.Exception.catching(classOf[NumberFormatException]) opt input.toInt
        if(idOpt.nonEmpty) {
          Await.result(get(MonsterID(idOpt.get,idOpt.get)), Duration(3, "seconds")).map{Right.apply}.getOrElse(Left(str))
        } else {
          Await.result(get(input), Duration(3, "seconds")).map{Right.apply}.getOrElse(Left(str))
        }
      case Left(x) => Left(x.msg)
      case Right(id) => Right(getMonster(id))
    }
  }
}
