package db.mongo

import data.Monster
import db.{DatabaseBackend, MonsterID}
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.{Cursor, MongoConnection, MongoDriver}
import reactivemongo.bson.BSONDocument

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * Created by heejong.lee on 3/15/16.
  */
trait MongoDBCache extends DatabaseBackend with MonsterMongoPickler {
  import scala.concurrent.ExecutionContext.Implicits.global

  val (uri,dbName) = Option(System.getenv("MONGODB_URL")).map{x => (x+"pazudorabot","pazudorabot")}.getOrElse("mongodb://localhost:27017","test")
  val driver = new MongoDriver
  val connection = MongoConnection.parseURI(uri).map{driver.connection(_)}
  val futureConnection = Future.fromTry(connection)
  def db = futureConnection.flatMap{_.database(dbName)}
  def collection = db.map{_.collection[BSONCollection]("pazudorabot")}

  override def getDBSize: Future[Int] = collection.flatMap{_.count()}
  override def clearMonsterCache(id: Option[Int]) : Future[Unit] = {
    if(id.nonEmpty) {
      collection.flatMap{_.remove(BSONDocument("id" -> id))}.flatMap{_ => Future.unit}
    } else collection.flatMap{_.drop()}
  }
  override def put(id: MonsterID, mon: Monster): Future[Unit] = {
    collection.flatMap{_.remove(BSONDocument("id" -> id.id))}.flatMap{ ret =>
      if(ret.ok) collection.flatMap{_.insert(mon)}.flatMap{_ => Future.unit}
      else Future.failed(new Exception(s"failed to remove ${mon.krName} from mongodb"))
    }.andThen {
      case Success(result) =>
        println(s"monster ${mon.krName} is successfully cached in mongodb: $result")
      case Failure(e) =>
        println(s"failed to save ${mon.krName} in mongodb: ${e.getMessage}")
    }
  }
  override def get(id: MonsterID): Future[Option[Monster]] = {
    val query = BSONDocument("id" -> id.id)
    val f = collection.flatMap{_.find(query).cursor[Monster]().collect[List](-1, Cursor.FailOnError())}
    f.map{_.headOption}.andThen {
      case Success(result) =>
        println(s"monster ID $id is successfully found in mongodb.")
      case Failure(e) =>
        println(s"exception thrown during mongodb find op: ${e.getMessage}")
    }
  }
  override def get(name: String): Future[Option[Monster]] = {
    val query = BSONDocument("krName" -> name)
    val f = collection.flatMap{_.find(query).cursor[Monster]().collect[List](-1, Cursor.FailOnError())}
    f.map{_.headOption}.andThen {
      case Success(result) =>
        println(s"monster name $name is successfully found in mongodb.")
      case Failure(e) =>
        println(s"exception thrown during mongodb find op: ${e.getMessage}")
    }
  }
}
