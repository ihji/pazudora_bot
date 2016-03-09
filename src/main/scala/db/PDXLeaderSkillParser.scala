package db

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

  val statement : Parser[LeaderSkill => LeaderSkill] = P(attrMag | tyMag | inputMag)
  val inputMag : Parser[LeaderSkill => LeaderSkill] =
    P(mag.rep(sep=",")).map{ case m => identity }
  val attrMag : Parser[LeaderSkill => LeaderSkill] =
    P(attr.rep(sep="&")~"attribute cards"~mag.rep(sep=",")).map{ case (a,m) => m.flatten.headOption.map{x => y : LeaderSkill => y.addAttrCond(a.toSet,x)}.getOrElse(identity) }
  val tyMag : Parser[LeaderSkill => LeaderSkill] =
    P(ty.rep(sep="&")~"type cards"~mag.rep(sep=",")).map{ case (t,m) => m.flatten.headOption.map{x => y : LeaderSkill => y.addTypeCond(t.toSet,x)}.getOrElse(identity) }
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
    val lskill = desc.split("\\. ").toSeq.foldLeft(new LeaderSkill){ case (l,d) =>
      statement.parse(d) match {
        case Result.Success(v,_) => v(l)
        case _ : Result.Failure =>
          println("failed to parse: "+d)
          l
      }
    }
    lskill
  }
}
