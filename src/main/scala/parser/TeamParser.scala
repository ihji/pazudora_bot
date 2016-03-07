package parser

import data.{Monster, Team}

/**
  * Created by ihji on 3/6/16.
  */
object TeamParser {
  def parseTeam(team: String) : Team = {
    team.split(",")
    ???
  }
  def genMonster(name: String) : Option[Monster] = {
    if(name.contains("+")) {
      val pair = name.split("+").map{_.trim}
      if(pair.length != 2) None
      else {
        val (name, plus) = (pair(0), pair(1))
        ???
      }
    } else {
      name.trim
      ???
    }
  }
}
