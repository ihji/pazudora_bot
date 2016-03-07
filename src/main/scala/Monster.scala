import Monster.Element

/**
  * Created by ihji on 3/6/16.
  */
case class Monster
(
  element: (Element,Option[Element]),
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
}