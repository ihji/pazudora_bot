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

  val statement : Parser[LeaderSkill => LeaderSkill] = P(
    attrMag | tyMag |
    comboMag | comboExtMag | combo1
  )
  val comboMag : Parser[LeaderSkill => LeaderSkill] =
    P(atkMag~"at"~num~"combos").map{
      case (a,m) => a.map{x => y : LeaderSkill => y.addComboCond(m.toInt,x)}.getOrElse(identity)
    }
  val comboExtMag : Parser[LeaderSkill => LeaderSkill] =
    P(atkMag~"for each additional combo, up to"~atkMag~"at"~num~"combos").map{
      case (step,a,m) => step.flatMap{s => a.map{x => y : LeaderSkill => y.addExtraComboCond(s,m.toInt,x)}}.getOrElse(identity)
    }
  val combo1 : Parser[LeaderSkill => LeaderSkill] =
    P("All attribute cards"~atkMag~"when reaching"~num~"combos or above").map{
      case (a,m) => a.map{x => y : LeaderSkill => y.addComboCond(m.toInt,x)}.getOrElse(identity)
    }
  val attrMag : Parser[LeaderSkill => LeaderSkill] =
    P(attr.rep(sep="&",min=1)~"attribute cards"~atkMag).map{
      case (a,m) => m.map{x => y : LeaderSkill => y.addAttrCond(a.toSet,x)}.getOrElse(identity)
    }
  val tyMag : Parser[LeaderSkill => LeaderSkill] =
    P(ty.rep(sep="&",min=1)~("type"|"attribute")~"cards"~atkMag).map{
      case (t,m) => m.map{x => y : LeaderSkill => y.addTypeCond(t.toSet,x)}.getOrElse(identity)
    }
  val atkMag : Parser[Option[Double]] =
    P(("ATK" | "HP" | "RCV").! ~ "x" ~ num).rep(sep=",",min=1).map{
      case ms => ms.find{_._1 == "ATK"}.map{_._2}
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
      val s = if(d.endsWith(".")) d.dropRight(1) else d
      statement.parse(s) match {
        case Result.Success(f,_) =>
          f(l)
        case x : Result.Failure =>
          println("failed to parse: "+s)
          l
      }
    }
    lskill
  }
}
