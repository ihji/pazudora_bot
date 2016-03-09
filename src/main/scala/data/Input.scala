package data

import data.Input.DropSet

/**
  * Created by heejong.lee on 3/7/16.
  */
case class Input(combo: Seq[DropSet]) {
  def add(d: DropSet) : Input = {
    this.copy(combo = combo :+ d)
  }
}

object Input {
  val empty = Input(Seq.empty)

  case class DropSet(kind: Drop, num: Int, isRow: Boolean = false, numEnhanced: Int = 0)

  sealed trait Drop
  case object Fire extends Drop { override def toString = "불" }
  case object Water extends Drop { override def toString = "물" }
  case object Wood extends Drop { override def toString = "나무" }
  case object Light extends Drop { override def toString = "빛" }
  case object Dark extends Drop { override def toString = "어둠" }
  case object Heart extends Drop { override def toString = "회복" }
  case object Jammer extends Drop { override def toString = "방해" }
}
