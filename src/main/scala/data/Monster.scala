package data

import data.Monster.{ActiveSkill, AwokenSkill, Element, Type}
import sem.DamageSimulator.Damage

/**
  * Created by ihji on 3/6/16.
  */
case class Monster
(
  id: Int,
  krName: String,
  jpName: String,
  thumbURL: String,
  picURL: String,
  ty: (Type,Option[Type],Option[Type]),
  star: Int,
  element: (Element,Option[Element]),
  hp: (Int,Int),
  atk: (Int,Int),
  rev: (Int,Int),
  cost: Int,
  maxLevel: Int,
  maxExp: Int,
  ranking: String,
  awokenSkill: Seq[AwokenSkill],
  choAwokenSkill: Seq[AwokenSkill],
  aSkill: Option[ActiveSkill],
  lSkill: Option[LeaderSkill],
  volatile: Boolean
) {
  val rating : Int = Math.round(hp._2 / 10.0 + atk._2 / 5.0 + rev._2 / 3.0).toInt
  def getNameString = {
    s"""No.$id *$krName* $jpName ${ty._1+ty._2.map{"/"+_}.getOrElse("")+ty._3.map{"/"+_}.getOrElse("")} ★$star ${element._1+element._2.map{"/"+_}.getOrElse("")}"""
  }
  def getStatString = {
    s"""*HP* ${hp._1} > ${hp._2} (+${hp._2 - hp._1})
       |*공격* ${atk._1} > ${atk._2} (+${atk._2 - atk._1})
       |*회복* ${rev._1} > ${rev._2} (+${rev._2 - rev._1})
       |*스킬* ${if(aSkill.nonEmpty) s"${aSkill.get.name} Lv.1 턴: ${aSkill.get.maxTurn} (Lv.${aSkill.get.maxLevel} 턴: ${aSkill.get.maxTurn - aSkill.get.maxLevel + 1})" else ""}
       |${if(aSkill.nonEmpty) aSkill.get.desc else "없음"}
       |*리더스킬* ${if(lSkill.nonEmpty) lSkill.get.name else ""}
       |${if(lSkill.nonEmpty) lSkill.get.krDesc else "없음"}
     """.stripMargin
  }
  def getInfoString = {
    s"""*HP* ${hp._1} > ${hp._2} (+${hp._2 - hp._1})
       |*공격* ${atk._1} > ${atk._2} (+${atk._2 - atk._1})
       |*회복* ${rev._1} > ${rev._2} (+${rev._2 - rev._1})
       |*코스트* $cost *최대레벨* $maxLevel *총경험치* ${Damage.friendly(maxExp)} *환산치* $rating
       |*각성스킬* ${if(awokenSkill.isEmpty) "없음" else awokenSkill.mkString(", ")}
       |*초각성스킬* ${if (choAwokenSkill.isEmpty) "없음" else choAwokenSkill.mkString(", ")}
       |
       |*스킬* ${if(aSkill.nonEmpty) s"${aSkill.get.name} Lv.1 턴: ${aSkill.get.maxTurn} (Lv.${aSkill.get.maxLevel} 턴: ${aSkill.get.maxTurn - aSkill.get.maxLevel + 1})" else ""}
       |${if(aSkill.nonEmpty) aSkill.get.desc else "없음"}
       |*리더스킬* ${if(lSkill.nonEmpty) lSkill.get.name else ""}
       |${if(lSkill.nonEmpty) lSkill.get.krDesc else "없음"}
     """.stripMargin
  }
  def getRanking = ranking
}

object Monster {
  case class ActiveSkill(name: String, maxTurn: Int, maxLevel: Int, desc: String)

  sealed trait Element { def toDrop : Input.Drop }
  case object Fire extends Element { override def toString = "불"; def toDrop = Input.Fire }
  case object Water extends Element { override def toString = "물"; def toDrop = Input.Water  }
  case object Wood extends Element { override def toString = "나무"; def toDrop = Input.Wood }
  case object Light extends Element { override def toString = "빛"; def toDrop = Input.Light  }
  case object Dark extends Element { override def toString = "어둠"; def toDrop = Input.Dark  }

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
  case object Evolution extends Type { override def toString = "진화용" }
  case object Awake extends Type { override def toString = "능력각성" }
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
  case object BindRes extends AwokenSkill { override def toString = "바인드내성" }
  case object BindImm extends AwokenSkill { override def toString = "바인드내성+" }
  case object JammerRes extends AwokenSkill { override def toString = "방해내성" }
  case object BlindRes extends AwokenSkill { override def toString = "암흑내성" }
  case object PoisonRes extends AwokenSkill { override def toString = "독내성" }
  case object SkillRes extends AwokenSkill { override def toString = "봉인내성" }
  case object BandRes extends AwokenSkill { override def toString = "조작불가내성" }
  case object CloudRes extends AwokenSkill { override def toString = "구름내성" }
  case object HpEn extends AwokenSkill { override def toString = "HP강화" }
  case object AtkEn extends AwokenSkill { override def toString = "공격강화" }
  case object RevEn extends AwokenSkill { override def toString = "회복강화" }
  case object TeamHpEn extends AwokenSkill { override def toString = "팀HP강화" }
  case object TeamRevEn extends AwokenSkill { override def toString = "팀회복력강화" }
  case object SkillBoost extends AwokenSkill { override def toString = "스킬부스트" }
  case object SkillBoostPlus extends AwokenSkill { override def toString = "스킬부스트+" }
  case object TwoWayAtk extends AwokenSkill { override def toString = "2마리공격" }
  case object ComboEn extends AwokenSkill { override def toString = "콤보강화" }
  case object ChoComboEn extends AwokenSkill { override def toString = "초콤보강화" }
  case object BonusAtk extends AwokenSkill { override def toString = "추가공격" }
  case object ChoBonusAtk extends AwokenSkill { override def toString = "초추가공격" }
  case object BindRec extends AwokenSkill { override def toString = "바인드회복" }
  case object AutoRec extends AwokenSkill { override def toString = "자동회복" }
  case object HeartDropEn extends AwokenSkill { override def toString = "회복드롭강화" }
  case object MultiBoost extends AwokenSkill { override def toString = "멀티부스터" }
  case object GuardBreak extends AwokenSkill { override def toString = "가드브레이크" }
  case object TimeExt extends AwokenSkill { override def toString = "조작시간연장" }
  case object TimeExtPlus extends AwokenSkill { override def toString = "조작시간연장+" }
  case object GodKiller extends AwokenSkill { override def toString = "신킬러" }
  case object MachineKiller extends AwokenSkill { override def toString = "머신킬러" }
  case object DevilKiller extends AwokenSkill { override def toString = "악마킬러" }
  case object DragonKiller extends AwokenSkill { override def toString = "드래곤킬러" }
  case object RevKiller extends AwokenSkill { override def toString = "회복킬러" }
  case object HpKiller extends AwokenSkill { override def toString = "체력킬러" }
  case object AtkKiller extends AwokenSkill { override def toString = "공격킬러" }
  case object BalKiller extends AwokenSkill { override def toString = "밸런스킬러" }
  case object AwokenKiller extends AwokenSkill { override def toString = "능력각성용킬러" }
  case object EnhanceKiller extends AwokenSkill { override def toString = "강화합성용킬러" }
  case object SellKiller extends AwokenSkill { override def toString = "판매용킬러" }
  case object EvolKiller extends AwokenSkill { override def toString = "진화용킬러" }
  case object DungeonBonus extends AwokenSkill { override def toString = "던전보너스" }
  case object DamageImmOff extends AwokenSkill { override def toString = "대미지무효관통" }
  case object Hp80UpEn extends AwokenSkill { override def toString = "HP80%이상강화" }
  case object Hp50DownEn extends AwokenSkill { override def toString = "HP50%이하강화" }
  case object LDep extends AwokenSkill { override def toString = "L자지우기경감" }
  case object LAtk extends AwokenSkill { override def toString = "L자지우기공격" }
  case object SkillCharge extends AwokenSkill { override def toString = "스킬충전" }
  case object AwokenAssist extends AwokenSkill { override def toString = "각성어시스트" }
  case object SkillVoice extends AwokenSkill { override def toString = "스킬보이스" }
  case object ComboDrop extends AwokenSkill { override def toString = "콤보드롭" }
  case object HpDem extends AwokenSkill { override def toString = "HP약화" }
  case object AtkDem extends AwokenSkill { override def toString = "공격약화" }
  case object RevDem extends AwokenSkill { override def toString = "회복약화" }

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
      case x if x == ComboEn.toString => ComboEn
      case x if x == ChoComboEn.toString => ChoComboEn
      case x if x == FireDep.toString => FireDep
      case x if x == WaterDep.toString => WaterDep
      case x if x == WoodDep.toString => WoodDep
      case x if x == LightDep.toString => LightDep
      case x if x == DarkDep.toString => DarkDep
      case x if x == JammerRes.toString => JammerRes
      case x if x == BindRes.toString => BindRes
      case x if x == BindImm.toString => BindImm
      case x if x == BlindRes.toString => BlindRes
      case x if x == PoisonRes.toString => PoisonRes
      case x if x == SkillRes.toString => SkillRes
      case x if x == CloudRes.toString => CloudRes
      case x if x == BandRes.toString => BandRes
      case x if x == HpEn.toString => HpEn
      case x if x == TeamHpEn.toString => TeamHpEn
      case x if x == AtkEn.toString => AtkEn
      case x if x == RevEn.toString => RevEn
      case x if x == TeamRevEn.toString => TeamRevEn
      case x if x == BindRec.toString => BindRec
      case x if x == AutoRec.toString => AutoRec
      case x if x == SkillBoost.toString => SkillBoost
      case x if x == SkillBoostPlus.toString => SkillBoostPlus
      case x if x == TwoWayAtk.toString => TwoWayAtk
      case x if x == BonusAtk.toString => BonusAtk
      case x if x == ChoBonusAtk.toString => ChoBonusAtk
      case x if x == TimeExt.toString => TimeExt
      case x if x == TimeExtPlus.toString => TimeExtPlus
      case x if x == HeartDropEn.toString => HeartDropEn
      case x if x == MultiBoost.toString => MultiBoost
      case x if x == GuardBreak.toString => GuardBreak
      case x if x == GodKiller.toString => GodKiller
      case x if x == MachineKiller.toString => MachineKiller
      case x if x == DevilKiller.toString => DevilKiller
      case x if x == DragonKiller.toString => DragonKiller
      case x if x == RevKiller.toString => RevKiller
      case x if x == HpKiller.toString => HpKiller
      case x if x == AtkKiller.toString => AtkKiller
      case x if x == BalKiller.toString => BalKiller
      case x if x == AwokenKiller.toString => AwokenKiller
      case x if x == EnhanceKiller.toString => EnhanceKiller
      case x if x == SellKiller.toString => SellKiller
      case x if x == EvolKiller.toString => EvolKiller
      case x if x == DungeonBonus.toString => DungeonBonus
      case x if x == DamageImmOff.toString => DamageImmOff
      case x if x == Hp80UpEn.toString => Hp80UpEn
      case x if x == Hp50DownEn.toString => Hp50DownEn
      case x if x == LDep.toString => LDep
      case x if x == LAtk.toString => LAtk
      case x if x == SkillCharge.toString => SkillCharge
      case x if x == AwokenAssist.toString => AwokenAssist
      case x if x == SkillVoice.toString => SkillVoice
      case x if x == ComboDrop.toString => ComboDrop
      case x if x == HpDem.toString => HpDem
      case x if x == AtkDem.toString => AtkDem
      case x if x == RevDem.toString => RevDem
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
      case _ if str == Awake.toString => Awake
      case _ if str == Point.toString => Point
    }
  }
}