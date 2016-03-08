package db

import data.LeaderSkill.{InputCond, TypeCond, AttrCond}
import data.{Monster, LeaderSkill}
import fastparse.WhitespaceApi
import fastparse.noApi._

/**
  * Created by ihji on 3/7/16.
  */
trait PDXLeaderSkillParser extends PDXParser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import White._

  val statement : Parser[Option[LeaderSkill.Condition]] = P(attrMag | tyMag | inputMag)
  val inputMag : Parser[Option[LeaderSkill.Condition]] =
    P(mag.rep(sep=",")).map{ case m => m.flatten.headOption.map{InputCond(_)} }
  val attrMag : Parser[Option[LeaderSkill.Condition]] =
    P(attr.rep(sep="&")~"attribute cards"~mag.rep(sep=",")).map{ case (a,m) => m.flatten.headOption.map{AttrCond(a.toSet,_)} }
  val tyMag : Parser[Option[LeaderSkill.Condition]] =
    P(ty.rep(sep="&")~"type cards"~mag.rep(sep=",")).map{ case (t,m) => m.flatten.headOption.map{TypeCond(t.toSet,_)} }
  val mag : Parser[Option[Double]] =
    P(("ATK" | "HP" | "RCV").! ~ "x" ~ num).map{
      case ("ATK",y) => Some(y)
      case _ => None
    }
  val ty : Parser[Monster.Type] =
    P("Devil").map{_ => Monster.Devil} |
    P("God").map{_ => Monster.God} |
    P("Attacker").map{_ => Monster.Attacker} |
    P("Physical").map{_ => Monster.Physical} |
    P("Healer").map{_ => Monster.Healer} |
    P("Balance").map{_ => Monster.Balance} |
    P("Dragon").map{_ => Monster.Dragon} |
    P("Machine").map{_ => Monster.Machine} |
    P("PowerUp").map{_ => Monster.PowerUp} |
    P("Evolution").map{_ => Monster.Evolution} |
    P("Point").map{_ => Monster.Point}
  val attr : Parser[Monster.Element] =
    P("Fire").map{_ => Monster.Fire} |
    P("Water").map{_ => Monster.Water} |
    P("Wood").map{_ => Monster.Wood} |
    P("Light").map{_ => Monster.Light} |
    P("Dark").map{_ => Monster.Dark}
  val num : Parser[Double] = P(CharIn('0' to '9', ".").repX(1).!).map{java.lang.Double.parseDouble}

  def getLeaderSkill(monId: MonsterID) : LeaderSkill = {
    val desc = getLSText(monId)
    val conds = desc.split("\\. ").toSeq.flatMap{ d =>
      statement.parse(d) match {
        case Result.Success(v,_) => v
        case _ : Result.Failure =>
          println("failed to parse: "+d)
          None
      }
    }
    LeaderSkill(conds)
  }
}
