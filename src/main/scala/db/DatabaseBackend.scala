package db

import data.Monster

import scala.concurrent.Future

/**
  * Created by heejong.lee on 3/15/16.
  */
trait DatabaseBackend {
  def getDBSize : Future[Int]
  def put(id: MonsterID, mon: Monster) : Future[Unit]
  def get(id: MonsterID) : Future[Option[Monster]]
  def get(name: String) : Future[Option[Monster]]
  def clearMonsterCache(id: Option[Int]) : Future[Unit]
}
