package data

import data.LeaderSkill.Condition

/**
  * Created by heejong.lee on 3/7/16.
  */
case class LeaderSkill(conds: Seq[Condition])

object LeaderSkill {
  sealed trait Condition
  case class InputCond(mag: Double) extends Condition
  case class AttrCond(attrs: Set[Monster.Element], mag: Double) extends Condition
  case class TypeCond(tys: Set[Monster.Type], mag: Double) extends Condition
}
