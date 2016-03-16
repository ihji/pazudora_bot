package db

import data.Monster
import db.mongo.MongoDBCache
import db.web.TIGSearch
import db.web.TIGSearch.NotFound

/**
  * Created by heejong.lee on 3/9/16.
  */
object MonsterDB extends MonsterParser with MongoDBCache with TIGSearch {
  def getMonster(id: MonsterID) : Monster = {
    val cachedMon = get(id)
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
        if(idOpt.nonEmpty) get(MonsterID(idOpt.get,idOpt.get)).map{Right.apply}.getOrElse(Left(str))
        else get(input).map{Right.apply}.getOrElse(Left(str))
      case Left(x) => Left(x.msg)
      case Right(id) => Right(getMonster(id))
    }
  }
}
