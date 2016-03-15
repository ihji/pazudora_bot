package db

import data.Monster
import db.mongo.MongoDBCache

/**
  * Created by heejong.lee on 3/9/16.
  */
object MonsterDB extends MonsterParser with MongoDBCache {
  def getMonster(id: MonsterID) : Monster = {
    val cachedMon = get(id)
    if(cachedMon.isEmpty) {
      val mon = parseMonster(id)
      put(id, mon)
      mon
    } else cachedMon.get
  }

}
