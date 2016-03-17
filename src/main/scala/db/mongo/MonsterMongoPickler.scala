package db.mongo

import data.{LeaderSkill, Monster}
import reactivemongo.bson._

/**
  * Created by heejong.lee on 3/15/16.
  */
trait MonsterMongoPickler {
  implicit object ActiveSkillWriter extends BSONDocumentWriter[Monster.ActiveSkill] {
    override def write(t: Monster.ActiveSkill): BSONDocument = BSONDocument(
      "name" -> t.name, "maxTurn" -> t.maxTurn, "maxLevel" -> t.maxLevel, "desc" -> t.desc
    )
  }
  implicit object ActiveSkillReader extends BSONDocumentReader[Monster.ActiveSkill] {
    override def read(bson: BSONDocument): Monster.ActiveSkill = Monster.ActiveSkill(
      bson.getAs[String]("name").get,
      bson.getAs[Int]("maxTurn").get,
      bson.getAs[Int]("maxLevel").get,
      bson.getAs[String]("desc").get
    )
  }
  implicit object LeaderSkillWriter extends BSONDocumentWriter[LeaderSkill] {
    override def write(t: LeaderSkill): BSONDocument = BSONDocument(
      "name" -> t.name, "krDesc" -> t.krDesc, "enDesc" -> t.enDesc
    )
  }
  implicit object LeaderReader extends BSONDocumentReader[LeaderSkill] {
    override def read(bson: BSONDocument): LeaderSkill = LeaderSkill(
      bson.getAs[String]("name").get,
      bson.getAs[String]("krDesc").get,
      bson.getAs[String]("enDesc").get
    )
  }
  implicit object MonsterWriter extends BSONDocumentWriter[Monster] {
    override def write(t: Monster): BSONDocument = BSONDocument(
      "id" -> t.id,
      "krName" -> t.krName,
      "jpName" -> t.jpName,
      "thumbURL" -> t.thumbURL,
      "picURL" -> t.picURL,
      "ty" -> List(Some(t.ty._1.toString), t.ty._2.map{_.toString}, t.ty._3.map{_.toString}).flatten,
      "star" -> t.star,
      "element" -> List(Some(t.element._1.toString),t.element._2.map{_.toString}).flatten,
      "hp" -> List(t.hp._1,t.hp._2),
      "atk" -> List(t.atk._1,t.atk._2),
      "rev" -> List(t.rev._1,t.rev._2),
      "cost" -> t.cost,
      "maxLevel" -> t.maxLevel,
      "maxExp" -> t.maxExp,
      "ranking" -> t.ranking,
      "awokenSkill" -> t.awokenSkill.map{_.toString},
      "aSkill" -> t.aSkill,
      "lSkill" -> t.lSkill,
      "volatile" -> t.volatile
    )
  }
  implicit object MonsterReader extends BSONDocumentReader[Monster] {
    override def read(bson: BSONDocument): Monster = Monster(
      bson.getAs[Int]("id").get,
      bson.getAs[String]("krName").get,
      bson.getAs[String]("jpName").get,
      bson.getAs[String]("thumbURL").get,
      bson.getAs[String]("picURL").get,
      bson.getAs[List[String]]("ty").map{ l =>
        l.length match {
          case 1 => (Monster.toType(l.head), None, None)
          case 2 => (Monster.toType(l.head), Some(Monster.toType(l(1))), None)
          case 3 => (Monster.toType(l.head), Some(Monster.toType(l(1))), Some(Monster.toType(l(2))))
          case _ => (Monster.God, None, None)
        }
      }.get,
      bson.getAs[Int]("star").get,
      bson.getAs[List[String]]("element").map{ l =>
        l.length match {
          case 1 => (Monster.toElem(l.head), None)
          case 2 => (Monster.toElem(l.head), Some(Monster.toElem(l(1))))
          case _ => (Monster.Fire, None)
        }
      }.get,
      bson.getAs[List[Int]]("hp").map{ l =>
        l.length match {
          case 2 => (l.head, l(1))
          case _ => (0, 0)
        }
      }.get,
      bson.getAs[List[Int]]("atk").map{ l =>
        l.length match {
          case 2 => (l.head, l(1))
          case _ => (0, 0)
        }
      }.get,
      bson.getAs[List[Int]]("rev").map{ l =>
        l.length match {
          case 2 => (l.head, l(1))
          case _ => (0, 0)
        }
      }.get,
      bson.getAs[Int]("cost").get,
      bson.getAs[Int]("maxLevel").get,
      bson.getAs[Int]("maxExp").get,
      bson.getAs[String]("ranking").get,
      bson.getAs[List[String]]("awokenSkill").get.map{Monster.toAwokenSkill},
      bson.getAs[Monster.ActiveSkill]("aSkill"),
      bson.getAs[LeaderSkill]("lSkill"),
      bson.getAs[Boolean]("volatile").getOrElse(false)
    )
  }
}
