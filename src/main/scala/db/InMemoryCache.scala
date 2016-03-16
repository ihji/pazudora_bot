package db

import data.Monster

/**
  * Created by heejong.lee on 3/15/16.
  */
trait InMemoryCache extends DatabaseBackend {
  var cache : Map[MonsterID,Monster] = Map.empty
  override def getDBSize: Int = cache.size
  override def put(id: MonsterID, mon: Monster): Unit = cache += id -> mon
  override def get(id: MonsterID): Option[Monster] = cache.get(id)
  override def get(name: String): Option[Monster] = cache.find{_._2.krName.contains(name)}.map{_._2}
}
