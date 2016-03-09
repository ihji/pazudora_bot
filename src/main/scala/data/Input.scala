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
  case object Fire extends Drop
  case object Water extends Drop
  case object Wood extends Drop
  case object Light extends Drop
  case object Dark extends Drop
  case object Heart extends Drop
  case object Jammer extends Drop
}
