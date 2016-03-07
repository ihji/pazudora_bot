package data

import data.Monster.{Element, Type}

/**
  * Created by ihji on 3/6/16.
  */
case class Monster
(
  element: (Element,Option[Element]),
  ty: (Type,Option[Type],Option[Type]),
  atk: Int,
  hp: Int,
  rev: Int,
  plusAtk: Int,
  plusHp: Int,
  plusRev: Int
)

object Monster {
  sealed trait Element
  case object Fire extends Element { override def toString = "불"}
  case object Water extends Element { override def toString = "물"}
  case object Wood extends Element { override def toString = "나무"}
  case object Light extends Element { override def toString = "빛"}
  case object Dark extends Element { override def toString = "어둠"}

  sealed trait Type
  case object Devil extends Type { override def toString = "악마"}
  case object God extends Type { override def toString = "신"}
  case object Attacker extends Type { override def toString = "공격"}
  case object Tanker extends Type { override def toString = "체력"}
  case object Healer extends Type { override def toString = "회복"}
  case object Balance extends Type { override def toString = "밸런스"}
  case object Dragon extends Type { override def toString = "드래곤"}
  case object Machine extends Type { override def toString = "머신"}
  case object PowerUp extends Type { override def toString = "강화합성용"}
  case object Evolution extends Type { override def toString = "진화소재"}
}