package db.mongo

import data.Monster
import db.{MonsterID, DatabaseBackend}
import reactivemongo.api.{MongoConnection, MongoDriver}
import reactivemongo.bson.BSONDocument

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure}

/**
  * Created by heejong.lee on 3/15/16.
  */
trait MongoDBCache extends DatabaseBackend with MonsterMongoPickler {
  val (uri,dbName) = Option(System.getenv("MONGODB_URL")).map{x => (x+"pazudorabot","pazudorabot")}.getOrElse("mongodb://localhost:27017","test")
  val driver = new MongoDriver
  val connection = MongoConnection.parseURI(uri).map{driver.connection}.get
  val db = connection(dbName)
  val collection = db("pazudorabot")

  override def getDBSize: Int = Await.result(collection.count(), Duration(3, "seconds"))
  override def clearMonsterCache(id: Option[Int]) {
    if(id.nonEmpty) {
      collection.remove(BSONDocument("id" -> id))
    } else collection.drop()
  }
  override def put(id: MonsterID, mon: Monster): Unit = {
    collection.remove(BSONDocument("id" -> id.id)).flatMap{ ret =>
      if(ret.ok) collection.insert(mon) else Future.failed(new Exception(s"failed to remove ${mon.krName} from mongodb"))
    }.onComplete {
      case Success(result) =>
        println(s"monster ${mon.krName} is successfully cached in mongodb: $result")
      case Failure(e) =>
        println(s"failed to save ${mon.krName} in mongodb: ${e.getMessage}")
    }
  }
  override def get(id: MonsterID): Option[Monster] = {
    val query = BSONDocument("id" -> id.id)
    val f = collection.find(query).cursor[Monster].collect[List]()
    try {
      Await.result(f,Duration(3, "seconds")).headOption
    } catch {
      case e: Throwable =>
        println(s"exception thrown during mongodb find op: ${e.getMessage}")
        None
    }
  }
  override def get(name: String): Option[Monster] = {
    val query = BSONDocument("krName" -> name)
    val f = collection.find(query).cursor[Monster].collect[List]()
    try {
      Await.result(f,Duration(3, "seconds")).headOption
    } catch {
      case e: Throwable =>
        println(s"exception thrown during mongodb find op: ${e.getMessage}")
        None
    }
  }
}
