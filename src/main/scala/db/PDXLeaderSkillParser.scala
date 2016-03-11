package db

import data.{Input, Monster, LeaderSkill}
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
    comboMag | comboExtMag | combo1 |
    fixedDropMag | flexDropMag | flexExtDropMag |
    numberMag | numberExtMag |
    enDropMag
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

  val fixedDropMag : Parser[LeaderSkill => LeaderSkill] =
    P(atkMag~"when attacking with"~drop.rep(sep=","|"&",min=1)~"orb types at the same time").map{
      case (a,d) => a.map{x => y : LeaderSkill => y.addFixedDropCond(d.toSet,x)}.getOrElse(identity)
    }
  val flexDropMag : Parser[LeaderSkill => LeaderSkill] =
    P(atkMag~"when attacking with"~num~"of following orb types:"~drop.rep(sep=","|"&",min=1)).map{
      case (a,n,d) => a.map{x => y : LeaderSkill => y.addFlexDropCond(d.toSet,n.toInt,x)}.getOrElse(identity)
    }
  val flexExtDropMag : Parser[LeaderSkill => LeaderSkill] =
    P(atkMag~"for each additional orb type, up to"~atkMag~"for all"~num~"matches").map{
      case (step,a,n) => step.flatMap{s => a.map{x => y : LeaderSkill => y.addExtraFlexDropCond(n.toInt,x,s)}}.getOrElse(identity)
    }

  val numberMag : Parser[LeaderSkill => LeaderSkill] =
    P(atkMag~"when simultaneously clearing"~num~"connected"~drop.rep(sep="or",min=1)~"orbs").map{
      case (a,n,d) => a.map{x => y : LeaderSkill => y.addNumberCond(d.toSet,n.toInt,x)}.getOrElse(identity)
    }
  val numberExtMag : Parser[LeaderSkill => LeaderSkill] =
    P(atkMag~"for each additional orb, up to"~atkMag~"at"~num~"connected orb").map{
      case (step,a,n) => step.flatMap{s => a.map{x => y : LeaderSkill => y.addExtraNumberCond(s,n.toInt,x)}}.getOrElse(identity)
    }

  val enDropMag : Parser[LeaderSkill => LeaderSkill] =
    P("Matched attribute"~atkMag~"when matching exactly"~num~"connected orbs with at least"~num~"enhanced orb").map{
      case (a,num,enNum) => a.map{x => y : LeaderSkill => y.addEnhDropCond(num.toInt, enNum.toInt, x)}.getOrElse(identity)
    }

  val attrMag : Parser[LeaderSkill => LeaderSkill] =
    P(attr.rep(sep=","|"&",min=1)~"attribute cards"~atkMag).map{
      case (a,m) => m.map{x => y : LeaderSkill => y.addAttrCond(a.toSet,x)}.getOrElse(identity)
    }
  val tyMag : Parser[LeaderSkill => LeaderSkill] =
    P(ty.rep(sep=","|"&",min=1)~("type"|"attribute")~"cards"~atkMag).map{
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
    P("Awake").map{_ => Monster.Awake} |
    P("Point").map{_ => Monster.Point}
  val attr : Parser[Monster.Element] =
    P("Fire").map{_ => Monster.Fire} |
    P("Water").map{_ => Monster.Water} |
    P("Wood").map{_ => Monster.Wood} |
    P("Light").map{_ => Monster.Light} |
    P("Dark").map{_ => Monster.Dark}
  val drop : Parser[Input.Drop] =
    P("Fire").map{_ => Input.Fire} |
    P("Water").map{_ => Input.Water} |
    P("Wood").map{_ => Input.Wood} |
    P("Light").map{_ => Input.Light} |
    P("Dark").map{_ => Input.Dark} |
    P("Heart").map{_ => Input.Heart} |
    P("Jammer").map{_ => Input.Jammer}
  val num : Parser[Double] = P(CharIn('0' to '9', ".").repX(1).!).map{java.lang.Double.parseDouble}

  def getLeaderSkill(monId: MonsterID, name: String, desc: String) : LeaderSkill = {
    val d = getLSText(monId)
    val lskill = d.split("\\. ").toSeq.foldLeft(new LeaderSkill(name,desc,d)){ case (l,d) =>
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
