package db

import data.Monster

/**
  * Created by heejong.lee on 3/15/16.
  */
trait DatabaseBackend {
  def getDBSize : Int
  def put(id: MonsterID, mon: Monster) : Unit
  def get(id: MonsterID) : Option[Monster]
  def get(name: String) : Option[Monster]
  def clearMonsterCache(id: Option[Int]) : Unit
}
