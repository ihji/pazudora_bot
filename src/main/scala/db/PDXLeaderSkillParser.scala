package db

import data.{Input, Monster, LeaderSkill}
import db.web.PDXParser

/**
  * Created by ihji on 3/7/16.
  */
trait PDXLeaderSkillParser extends PDXParser { self: LeaderSkill =>
  import fastparse._, NoWhitespace._

  def statement[_ : P] : P[LeaderSkill => LeaderSkill] = P(
    comboMag | comboExtMag | combo1 |
    fixedDropMag | flexDropMag | flexExtDropMag |
    numberMag | numberExtMag |
    enDropMag |
    hpCondLtMag | hpCondGtMag | hpCondFullMag |
    fixedComboMag | fixedComboMag2 | fixedExtraComboMag |
    attrMag | tyMag
  )

  def comboMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P(atkMag~"at"~num~"combos").map{
      case (a,m) => _.addAtkComboCond(m.toInt,a)
    }
  def comboExtMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P(atkMag~"for each additional combo, up to"~atkMag~"at"~num~"combos").map{
      case (step,a,m) => _.addAtkExtraComboCond(step,m.toInt,a)
    }
  def combo1[_ : P] : P[LeaderSkill => LeaderSkill] =
    P("All attribute cards"~atkMag~"when reaching"~num~"combos or above").map{
      case (a,m) => _.addAtkComboCond(m.toInt,a)
    }

  def fixedDropMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P(atkMag~"when attacking with"~drop.rep(sep=","|"&",min=1)~"orb types at the same time").map{
      case (a,d) => _.addAtkFixedDropCond(d.toSet,a)
    }
  def flexDropMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P(atkMag~"when attacking with"~num~"of following orb types:"~drop.rep(sep=","|"&",min=1)).map{
      case (a,n,d) => _.addAtkFlexDropCond(d.toSet,n.toInt,a)
    }
  def flexExtDropMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P(atkMag~"for each additional orb type, up to"~atkMag~"for all"~num~"matches").map{
      case (step,a,n) => _.addAtkExtraFlexDropCond(n.toInt,a,step)
    }

  def numberMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P(atkMag~"when simultaneously clearing"~num~"connected"~drop.rep(sep="or",min=1)~"orbs").map{
      case (a,n,d) => _.addAtkNumberCond(d.toSet,n.toInt,a)
    }
  def numberExtMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P(atkMag~"for each additional orb, up to"~atkMag~"at"~num~"connected orb").map{
      case (step,a,n) => _.addAtkExtraNumberCond(step,n.toInt,a)
    }

  def enDropMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P("Matched attribute"~atkMag~"when matching exactly"~num~"connected orbs with at least"~num~"enhanced orb").map{
      case (a,num,enNum) => _.addAtkEnhDropCond(num.toInt, enNum.toInt, a)
    }

  def hpCondGtMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P((attr.rep(sep=","|"&",min=1)~"attribute cards").? ~atkMag~"when HP is greater than"~num~"%").map {
      case (a,m,n) => _.addAtkHpCondMoreThan(a.map{_.toSet},n.toInt,m)
    }
  def hpCondLtMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P((attr.rep(sep=","|"&",min=1)~"attribute cards").? ~atkMag~"when HP is less than"~num~"%").map {
      case (a,m,n) => _.addAtkHpCondLessThan(a.map{_.toSet},n.toInt,m)
    }
  def hpCondFullMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P((attr.rep(sep=","|"&",min=1)~"attribute cards").? ~atkMag~"when HP is full").map {
      case (a,m) => _.addAtkHpCondMoreThan(a.map{_.toSet},100,m)
    }

  def fixedComboMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P("All attribute cards"~atkMag~"when reaching"~drop.rep(sep=","|"&",min=1).rep(sep="or")~"combos").map{
      case (a,drops) => _.addAtkFixedComboCond(drops.toSet,a)
    }
  def fixedComboMag2[_ : P] : P[LeaderSkill => LeaderSkill] =
    P("All attribute cards"~atkMag~"when reaching"~num~"set of"~drop~("combo"|"combos")).map{
      case (a,n,d) => _.addAtkFixedComboCond(Set((0 until n.toInt).map{ _=>d}),a)
    }
  def fixedExtraComboMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P(atkMag~"for each additional combo, up to"~atkMag~"when reaching"~drop.rep(sep=","|"&",min=1).? ~num.? ~"combos"~"combination".?).map{
      case (step,a,drops,num) =>
        _.addAtkExtraFixedComboCond(step,a, if(drops.nonEmpty) Left(Set(drops.get)) else if(num.nonEmpty) Right(num.get.toInt) else Left(Set()))
    }

  def attrMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P(attr.rep(sep=","|"&",min=1)~"attribute cards"~statMag).map{
      case (a,m) =>
        m.get("ATK").map{x => y : LeaderSkill => y.addAtkAttrCond(a.toSet,x)}.getOrElse{x : LeaderSkill => x} andThen
        m.get("HP").map{x => y : LeaderSkill => y.addHpAttrCond(a.toSet,x)}.getOrElse{x : LeaderSkill => x} andThen
        m.get("RCV").map{x => y : LeaderSkill => y.addRevAttrCond(a.toSet,x)}.getOrElse{x : LeaderSkill => x}
    }
  def tyMag[_ : P] : P[LeaderSkill => LeaderSkill] =
    P(ty.rep(sep=","|"&",min=1)~("type"|"attribute")~"cards"~statMag).map{
      case (t,m) =>
        m.get("ATK").map{x => y : LeaderSkill => y.addAtkTypeCond(t.toSet,x)}.getOrElse{x : LeaderSkill => x} andThen
        m.get("HP").map{x => y : LeaderSkill => y.addHpTypeCond(t.toSet,x)}.getOrElse{x : LeaderSkill => x} andThen
        m.get("RCV").map{x => y : LeaderSkill => y.addRevTypeCond(t.toSet,x)}.getOrElse{x : LeaderSkill => x}
    }

  def atkMag[_ : P] : P[Double] = P("ATK" ~ "x" ~ num)
  def statMag[_ : P] : P[Map[String,Double]] =
    P(("ATK" | "HP" | "RCV").! ~ "x" ~ num).rep(sep=",",min=1).map{_.toMap}
  def ty[_ : P] : P[Monster.Type] =
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
  def attr[_ : P] : P[Monster.Element] =
    P("Fire").map{_ => Monster.Fire} |
    P("Water").map{_ => Monster.Water} |
    P("Wood").map{_ => Monster.Wood} |
    P("Light").map{_ => Monster.Light} |
    P("Dark").map{_ => Monster.Dark}
  def drop[_ : P] : P[Input.Drop] =
    P("Fire").map{_ => Input.Fire} |
    P("Water").map{_ => Input.Water} |
    P("Wood").map{_ => Input.Wood} |
    P("Light").map{_ => Input.Light} |
    P("Dark").map{_ => Input.Dark} |
    P("Heart").map{_ => Input.Heart} |
    P("Jammer").map{_ => Input.Jammer}
  def num[_ : P] : P[Double] = P(CharIn("0-9", ".").repX(1).!).map{java.lang.Double.parseDouble}

  def genLeaderSkill() : LeaderSkill = {
    enDesc.split("\\. |\\(|\\)").toSeq.foldLeft(this){ case (l,d) =>
      val s = if(d.endsWith(".")) d.dropRight(1) else d
      parse(s.trim, statement(_)) match {
        case Parsed.Success(f,_) =>
          f(l)
        case x : Parsed.Failure =>
          println("failed to parse: "+s)
          l
      }
    }
  }
}
