package db

import data.Monster

import scala.concurrent.Future

/**
  * Created by heejong.lee on 3/15/16.
  */
trait InMemoryCache extends DatabaseBackend {
  var cache : Map[MonsterID,Monster] = Map.empty
  override def getDBSize: Future[Int] = Future.successful(cache.size)
  override def put(id: MonsterID, mon: Monster): Future[Unit] = Future.successful(cache += id -> mon)
  override def get(id: MonsterID): Future[Option[Monster]] = Future.successful(cache.get(id))
  override def get(name: String): Future[Option[Monster]] = Future.successful(cache.find{_._2.krName.contains(name)}.map{_._2})
  override def clearMonsterCache(id: Option[Int]): Future[Unit] =
    Future.successful(if(id.nonEmpty) cache = cache.filterNot{_._1.id == id.get} else cache = Map.empty)
}
