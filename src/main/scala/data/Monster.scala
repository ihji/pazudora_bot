package data

import data.Monster.{AwokenSkill, Element, Type}

/**
  * Created by ihji on 3/6/16.
  */
case class Monster
(
  id: Int,
  element: (Element,Option[Element]),
  ty: (Type,Option[Type],Option[Type]),
  atk: Int,
  hp: Int,
  rev: Int,
  awokenSkill: Seq[AwokenSkill],
  plusAtk: Int = 0,
  plusHp: Int = 0,
  plusRev: Int = 0
)

object Monster {
  sealed trait Element
  case object Fire extends Element { override def toString = "불" }
  case object Water extends Element { override def toString = "물" }
  case object Wood extends Element { override def toString = "나무" }
  case object Light extends Element { override def toString = "빛" }
  case object Dark extends Element { override def toString = "어둠" }

  sealed trait Type
  case object Devil extends Type { override def toString = "악마" }
  case object God extends Type { override def toString = "신" }
  case object Attacker extends Type { override def toString = "공격" }
  case object Physical extends Type { override def toString = "체력" }
  case object Healer extends Type { override def toString = "회복" }
  case object Balance extends Type { override def toString = "밸런스" }
  case object Dragon extends Type { override def toString = "드래곤" }
  case object Machine extends Type { override def toString = "머신" }
  case object PowerUp extends Type { override def toString = "강화합성용" }
  case object Evolution extends Type { override def toString = "진화소재" }
  case object Point extends Type { override def toString = "매각용" }

  sealed trait AwokenSkill
  case object FireDropEn extends AwokenSkill { override def toString = "불드롭강화" }
  case object WaterDropEn extends AwokenSkill { override def toString = "물드롭강화" }
  case object WoodDropEn extends AwokenSkill { override def toString = "나무드롭강화" }
  case object LightDropEn extends AwokenSkill { override def toString = "빛드롭강화" }
  case object DarkDropEn extends AwokenSkill { override def toString = "어둠드롭강화" }
  case object FireEn extends AwokenSkill { override def toString = "불속성강화" }
  case object WaterEn extends AwokenSkill { override def toString = "물속성강화" }
  case object WoodEn extends AwokenSkill { override def toString = "나무속성강화" }
  case object LightEn extends AwokenSkill { override def toString = "빛속성강화" }
  case object DarkEn extends AwokenSkill { override def toString = "어둠속성강화" }
  case object FireDep extends AwokenSkill { override def toString = "불대미지경감" }
  case object WaterDep extends AwokenSkill { override def toString = "물대미지경감" }
  case object WoodDep extends AwokenSkill { override def toString = "나무대미지경감" }
  case object LightDep extends AwokenSkill { override def toString = "빛대미지경감" }
  case object DarkDep extends AwokenSkill { override def toString = "어둠대미지경감" }
  case object JammerRes extends AwokenSkill { override def toString = "방해내성" }
  case object BindRes extends AwokenSkill { override def toString = "바인딩내성" }
  case object BlindRes extends AwokenSkill { override def toString = "암흑내성" }
  case object PoisonRes extends AwokenSkill { override def toString = "독내성" }
  case object SkillRes extends AwokenSkill { override def toString = "봉인내성" }
  case object HpEn extends AwokenSkill { override def toString = "HP강화" }
  case object AtkEn extends AwokenSkill { override def toString = "공격강화" }
  case object RevEn extends AwokenSkill { override def toString = "회복강화" }
  case object BindRec extends AwokenSkill { override def toString = "바인드회복" }
  case object AutoRec extends AwokenSkill { override def toString = "자동회복" }
  case object SkillBoost extends AwokenSkill { override def toString = "스킬부스트" }
  case object TwoWayAtk extends AwokenSkill { override def toString = "2마리공격" }
  case object TimeExt extends AwokenSkill { override def toString = "조작시간연장" }
  case object HeartDropEn extends AwokenSkill { override def toString = "회복드롭강화" }
  case object MultiBoost extends AwokenSkill { override def toString = "멀티부스터" }
  case object GodKiller extends AwokenSkill { override def toString = "신킬러" }
  case object MachineKiller extends AwokenSkill { override def toString = "머신킬러" }
  case object DevilKiller extends AwokenSkill { override def toString = "악마킬러" }
  case object DragonKiller extends AwokenSkill { override def toString = "드래곤킬러" }

  def toAwokenSkill(str: String) : AwokenSkill = {
    str.replaceAll(" ","") match {
      case x if x == FireDropEn.toString => FireDropEn
      case x if x == WaterDropEn.toString => WaterDropEn
      case x if x == WoodDropEn.toString => WoodDropEn
      case x if x == LightDropEn.toString => LightDropEn
      case x if x == DarkDropEn.toString => DarkDropEn
      case x if x == FireEn.toString => FireEn
      case x if x == WaterEn.toString => WaterEn
      case x if x == WoodEn.toString => WoodEn
      case x if x == LightEn.toString => LightEn
      case x if x == DarkEn.toString => DarkEn
      case x if x == FireDep.toString => FireDep
      case x if x == WaterDep.toString => WaterDep
      case x if x == WoodDep.toString => WoodDep
      case x if x == LightDep.toString => LightDep
      case x if x == DarkDep.toString => DarkDep
      case x if x == JammerRes.toString => JammerRes
      case x if x == BindRes.toString => BindRes
      case x if x == BlindRes.toString => BlindRes
      case x if x == PoisonRes.toString => PoisonRes
      case x if x == SkillRes.toString => SkillRes
      case x if x == HpEn.toString => HpEn
      case x if x == AtkEn.toString => AtkEn
      case x if x == RevEn.toString => RevEn
      case x if x == BindRec.toString => BindRec
      case x if x == AutoRec.toString => AutoRec
      case x if x == SkillBoost.toString => SkillBoost
      case x if x == TwoWayAtk.toString => TwoWayAtk
      case x if x == TimeExt.toString => TimeExt
      case x if x == HeartDropEn.toString => HeartDropEn
      case x if x == MultiBoost.toString => MultiBoost
      case x if x == GodKiller.toString => GodKiller
      case x if x == MachineKiller.toString => MachineKiller
      case x if x == DevilKiller.toString => DevilKiller
      case x if x == DragonKiller.toString => DragonKiller
    }
  }

  def toElem(str: String) : Element = {
    str match {
      case _ if str == Fire.toString => Fire
      case _ if str == Water.toString => Water
      case _ if str == Wood.toString => Wood
      case _ if str == Light.toString => Light
      case _ if str == Dark.toString => Dark
    }
  }

  def toType(str: String) : Type = {
    str match {
      case _ if str == Devil.toString => Devil
      case _ if str == God.toString => God
      case _ if str == Attacker.toString => Attacker
      case _ if str == Physical.toString => Physical
      case _ if str == Healer.toString => Healer
      case _ if str == Balance.toString => Balance
      case _ if str == Dragon.toString => Dragon
      case _ if str == Machine.toString => Machine
      case _ if str == PowerUp.toString => PowerUp
      case _ if str == Evolution.toString => Evolution
      case _ if str == Point.toString => Point
    }
  }
}