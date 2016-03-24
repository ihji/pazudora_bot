package data

import java.util.Date

import data.RareEggs.MonsterKind

import scala.util.Random

/**
  * Created by heejong.lee on 3/22/16.
  */
case class RareEggs(carnival: Option[Monster.Element], godFest: Option[Seq[MonsterKind]], excludeGodFestLimited: Set[Int]) {
  var CARNIVAL_BENEFIT = 3
  var GODFEST_LIMITED_BENEFIT = 3
  var GODFEST_BENEFIT = 3

  val STAR5_PENALTY = 2
  val STAR4_PENALTY = 3

  val random = new Random(new Date().getTime())

  lazy val targets = {
    val carnivalMonsters = carnival.map{e => RareEggs.rareEggMonsters.filter{x => x.rarity == 5 && x.elem == e}}.getOrElse(Seq())
    val carnivalLimited = carnival.map{e => RareEggs.rareEggMonstersCarnivalLimited.filter{x => x.rarity == 5 && x.elem == e}}.getOrElse(Seq())
    val godFestMonsters = godFest.map{_.flatMap{k => RareEggs.rareEggMonsters.filter{_.kind == k}}}.getOrElse(Seq())
    val godFestLimiteds = if(godFest.nonEmpty) RareEggs.rareEggMonstersGodFestLimited.filterNot{x => excludeGodFestLimited(x.id)} else Seq()

    val allMonsters = RareEggs.rareEggMonsters ++ godFestLimiteds ++ carnivalLimited ++
      (1 until CARNIVAL_BENEFIT).flatMap{_ => carnivalMonsters ++ carnivalLimited} ++
      (1 until GODFEST_LIMITED_BENEFIT).flatMap{_ => godFestLimiteds} ++
      (1 until GODFEST_BENEFIT).flatMap{_ => godFestMonsters}

    val allMonsters5 = allMonsters.filter{_.rarity == 5}
    val allMonsters4 = allMonsters.filter{_.rarity == 4}

    allMonsters ++
      (1 until STAR5_PENALTY).flatMap{_ => allMonsters5} ++
      (1 until STAR4_PENALTY).flatMap{_ => allMonsters4}
  }.map{_.id}

  def pick() : Int = {
    val idx = random.nextInt(targets.length)
    targets(idx)
  }
}

object RareEggs {
  case class Egg(id: Int, rarity: Int, elem: Monster.Element, kind: MonsterKind)

  val rareEggMonstersCarnivalLimited = Seq(
    Egg(2665, 5, Monster.Fire, CarnivalLimited),
    Egg(2667, 5, Monster.Water, CarnivalLimited),
    Egg(2669, 5, Monster.Wood, CarnivalLimited),
    Egg(2671, 5, Monster.Light, CarnivalLimited),
    Egg(2673, 5, Monster.Dark, CarnivalLimited)
  )

  val rareEggMonstersGodFestLimited = Seq(
    Egg(362, 5, Monster.Wood, GodFestLimited), // 녹딘
    Egg(364, 6, Monster.Water, GodFestLimited), // 청딘
    Egg(640, 5, Monster.Light, GodFestLimited), // 백타
    Egg(642, 6, Monster.Dark, GodFestLimited), // 흑타
    Egg(911, 5, Monster.Fire, GodFestLimited), // 적소
    Egg(913, 6, Monster.Wood, GodFestLimited), // 녹소
    Egg(1088, 6, Monster.Water, GodFestLimited), // 청소
    Egg(1107, 6, Monster.Fire, GodFestLimited), // 적딘
    Egg(1585, 5, Monster.Light, GodFestLimited), // 빛칼리
    Egg(1587, 6, Monster.Dark, GodFestLimited), // 암칼리
    Egg(1669, 5, Monster.Fire, GodFestLimited), // 울드
    Egg(1671, 5, Monster.Wood, GodFestLimited), // 베르단디
    Egg(1673, 6, Monster.Water, GodFestLimited), // 스쿨드
    Egg(1946, 5, Monster.Fire, GodFestLimited), // 가디우스
    Egg(1948, 6, Monster.Dark, GodFestLimited), // 티폰
    Egg(1950, 6, Monster.Light, GodFestLimited), // 이르무
    Egg(1952, 5, Monster.Dark, GodFestLimited), // 즈오
    Egg(2141, 5, Monster.Fire, GodFestLimited), // 츠바키
    Egg(2143, 5, Monster.Water, GodFestLimited), // 스미레
    Egg(2145, 6, Monster.Wood, GodFestLimited), // 카에데
    Egg(2147, 6, Monster.Light, GodFestLimited), // 칸나
    Egg(2149, 5, Monster.Dark, GodFestLimited), // 사츠키
    Egg(2440, 5, Monster.Light, GodFestLimited), // 사리아
    Egg(2442, 6, Monster.Water, GodFestLimited), // 류네
    Egg(2508, 6, Monster.Wood, GodFestLimited), // 실비
    Egg(2562, 5, Monster.Wood, GodFestLimited), // 아우스트라리스
    Egg(2564, 6, Monster.Water, GodFestLimited), // 쉐아트
    Egg(2591, 6, Monster.Dark, GodFestLimited), // 에스카마리
    Egg(2640, 5, Monster.Dark, GodFestLimited) // 펜릴
  )
  val rareEggMonsters = Seq(
    Egg(1270, 6, Monster.Fire, Valkyries),
    Egg(972, 6, Monster.Water, Valkyries),
    Egg(1516, 6, Monster.Wood, Valkyries),
    Egg(982, 6, Monster.Dark, Valkyries),
    Egg(1356, 6, Monster.Fire, PDZCollabs),
    Egg(1131, 6, Monster.Wood, PDZCollabs),
    Egg(1358, 6, Monster.Dark, PDZCollabs),
    Egg(379, 6, Monster.Fire, Riders),
    Egg(381, 6, Monster.Water, Riders),
    Egg(383, 6, Monster.Wood, Riders),
    Egg(385, 6, Monster.Light, Riders),
    Egg(387, 6, Monster.Dark, Riders),
    Egg(556, 6, Monster.Fire, Ninjas),
    Egg(558, 6, Monster.Water, Ninjas),
    Egg(560, 6, Monster.Wood, Ninjas),
    Egg(562, 6, Monster.Light, Ninjas),
    Egg(564, 6, Monster.Dark, Ninjas),
    Egg(1243, 6, Monster.Fire, Expelleds), // 불관우
    Egg(1374, 6, Monster.Light, Expelleds), // 빛갈량

    Egg(2552, 5, Monster.Fire, Constellations2),
    Egg(2554, 5, Monster.Water, Constellations2),
    Egg(2556, 5, Monster.Wood, Constellations2),
    Egg(2558, 5, Monster.Light, Constellations2),
    Egg(2560, 5, Monster.Dark, Constellations2),

    Egg(2415, 5, Monster.Fire, Constellations),
    Egg(2417, 5, Monster.Water, Constellations),
    Egg(2419, 5, Monster.Wood, Constellations),
    Egg(2421, 5, Monster.Light, Constellations),
    Egg(2423, 5, Monster.Dark, Constellations),

    Egg(2264, 5, Monster.Fire, Generals),
    Egg(2266, 5, Monster.Water, Generals),
    Egg(2268, 5, Monster.Wood, Generals),
    Egg(2270, 5, Monster.Light, Generals),
    Egg(2272, 5, Monster.Dark, Generals),

    Egg(2190, 5, Monster.Fire, MachineAnimals),
    Egg(2191, 5, Monster.Water, MachineAnimals),
    Egg(2192, 5, Monster.Wood, MachineAnimals),
    Egg(2193, 5, Monster.Light, MachineAnimals),
    Egg(2194, 5, Monster.Dark, MachineAnimals),

    Egg(2185, 5, Monster.Fire, MachineDragons),
    Egg(2186, 5, Monster.Water, MachineDragons),
    Egg(2187, 5, Monster.Wood, MachineDragons),
    Egg(2188, 5, Monster.Light, MachineDragons),
    Egg(2189, 5, Monster.Dark, MachineDragons),

    Egg(1503, 5, Monster.Fire, DragonKnights),
    Egg(1505, 5, Monster.Water, DragonKnights),
    Egg(1897, 5, Monster.Wood, DragonKnights),
    Egg(1507, 5, Monster.Light, DragonKnights),
    Egg(1899, 5, Monster.Dark, DragonKnights),

    Egg(1413, 5, Monster.Fire, FairyTales),
    Egg(1415, 5, Monster.Water, FairyTales),
    Egg(1417, 5, Monster.Wood, FairyTales),
    Egg(1419, 5, Monster.Light, FairyTales),
    Egg(1421, 5, Monster.Dark, FairyTales),

    Egg(1881, 5, Monster.Fire, Weapons),
    Egg(1882, 5, Monster.Water, Weapons),
    Egg(1883, 5, Monster.Wood, Weapons),
    Egg(1884, 5, Monster.Light, Weapons),
    Egg(1885, 5, Monster.Dark, Weapons),

    Egg(1826, 5, Monster.Fire, Angels2),
    Egg(1828, 5, Monster.Water, Angels2),
    Egg(1830, 5, Monster.Wood, Angels2),
    Egg(1832, 5, Monster.Light, Angels2),
    Egg(1834, 5, Monster.Dark, Angels2),

    Egg(1755, 5, Monster.Light, MagicBooks),
    Egg(1757, 5, Monster.Dark, MagicBooks),

    Egg(415, 5, Monster.Fire, Girls),
    Egg(418, 5, Monster.Water, Girls),
    Egg(421, 5, Monster.Wood, Girls),
    Egg(424, 5, Monster.Light, Girls),
    Egg(427, 5, Monster.Dark, Girls),

    Egg(1649, 5, Monster.Fire, ArmedKnights),
    Egg(1651, 5, Monster.Water, ArmedKnights),
    Egg(1653, 5, Monster.Wood, ArmedKnights),
    Egg(1655, 5, Monster.Light, ArmedKnights),
    Egg(1657, 5, Monster.Dark, ArmedKnights),

    Egg(1614, 5, Monster.Fire, Magicians),
    Egg(1616, 5, Monster.Water, Magicians),
    Egg(1618, 5, Monster.Wood, Magicians),
    Egg(1620, 5, Monster.Light, Magicians),
    Egg(1622, 5, Monster.Dark, Magicians),

    Egg(1706, 5, Monster.Fire, Magicians),
    Egg(1626, 5, Monster.Dark, Magicians),
    Egg(1704, 5, Monster.Wood, Magicians),
    Egg(1624, 5, Monster.Light, Magicians),

    Egg(1330, 5, Monster.Fire, IndianGods2),
    Egg(1332, 5, Monster.Water, IndianGods2),
    Egg(1334, 5, Monster.Wood, IndianGods2),
    Egg(1336, 5, Monster.Light, IndianGods2),
    Egg(1338, 5, Monster.Dark, IndianGods2),

    Egg(1121, 5, Monster.Fire, PDZCollabs),
    Egg(1123, 5, Monster.Water, PDZCollabs),
    Egg(1125, 5, Monster.Wood, PDZCollabs),
    Egg(1127, 5, Monster.Light, PDZCollabs),
    Egg(1129, 5, Monster.Dark, PDZCollabs),

    Egg(1350, 5, Monster.Fire, PDZCollabs),
    Egg(1130, 5, Monster.Wood, PDZCollabs),
    Egg(1352, 5, Monster.Wood, PDZCollabs),
    Egg(1354, 5, Monster.Light, PDZCollabs),
    Egg(1355, 5, Monster.Fire, PDZCollabs),
    Egg(1357, 5, Monster.Dark, PDZCollabs),

    Egg(1231, 5, Monster.Fire, ThreeKingdomGods),
    Egg(1233, 5, Monster.Water, ThreeKingdomGods),
    Egg(1235, 5, Monster.Wood, ThreeKingdomGods),
    Egg(1237, 5, Monster.Light, ThreeKingdomGods),
    Egg(1239, 5, Monster.Dark, ThreeKingdomGods),

    Egg(1065, 5, Monster.Fire, HeroGods),
    Egg(1067, 5, Monster.Water, HeroGods),
    Egg(1069, 5, Monster.Wood, HeroGods),
    Egg(1071, 5, Monster.Light, HeroGods),
    Egg(1073, 5, Monster.Dark, HeroGods),

    Egg(113, 5, Monster.Fire, LateBloomerDragons),
    Egg(115, 5, Monster.Water, LateBloomerDragons),
    Egg(117, 5, Monster.Wood, LateBloomerDragons),
    Egg(119, 5, Monster.Light, LateBloomerDragons),
    Egg(121, 5, Monster.Dark, LateBloomerDragons),

    Egg(1076, 5, Monster.Fire, FruitDragons),
    Egg(1078, 5, Monster.Water, FruitDragons),
    Egg(1080, 5, Monster.Wood, FruitDragons),
    Egg(1082, 5, Monster.Light, FruitDragons),
    Egg(1084, 5, Monster.Dark, FruitDragons),

    Egg(122, 5, Monster.Fire, GreekRomanGods),
    Egg(124, 5, Monster.Water, GreekRomanGods),
    Egg(126, 5, Monster.Wood, GreekRomanGods),
    Egg(128, 5, Monster.Light, GreekRomanGods),
    Egg(130, 5, Monster.Dark, GreekRomanGods),

    Egg(132, 5, Monster.Fire, JapaneseGods),
    Egg(134, 5, Monster.Water, JapaneseGods),
    Egg(136, 5, Monster.Wood, JapaneseGods),
    Egg(138, 5, Monster.Light, JapaneseGods),
    Egg(140, 5, Monster.Dark, JapaneseGods),

    Egg(236, 5, Monster.Fire, IndianGods),
    Egg(238, 5, Monster.Water, IndianGods),
    Egg(240, 5, Monster.Wood, IndianGods),
    Egg(242, 5, Monster.Light, IndianGods),
    Egg(244, 5, Monster.Dark, IndianGods),

    Egg(353, 5, Monster.Fire, Elementals),
    Egg(355, 5, Monster.Water, Elementals),
    Egg(357, 5, Monster.Wood, Elementals),
    Egg(359, 5, Monster.Light, Elementals),
    Egg(361, 5, Monster.Dark, Elementals),

    Egg(368, 5, Monster.Fire, NorthernEuropeGods),
    Egg(370, 5, Monster.Water, NorthernEuropeGods),
    Egg(372, 5, Monster.Wood, NorthernEuropeGods),
    Egg(374, 5, Monster.Light, NorthernEuropeGods),
    Egg(376, 5, Monster.Dark, NorthernEuropeGods),

    Egg(378, 5, Monster.Fire, Riders),
    Egg(380, 5, Monster.Water, Riders),
    Egg(382, 5, Monster.Wood, Riders),
    Egg(384, 5, Monster.Light, Riders),
    Egg(386, 5, Monster.Dark, Riders),

    Egg(490, 5, Monster.Fire, EgyptianGods),
    Egg(492, 5, Monster.Water, EgyptianGods),
    Egg(494, 5, Monster.Wood, EgyptianGods),
    Egg(496, 5, Monster.Light, EgyptianGods),
    Egg(498, 5, Monster.Dark, EgyptianGods),

    Egg(1659, 5, Monster.Fire, EgyptianGods2),
    Egg(1661, 5, Monster.Water, EgyptianGods2),
    Egg(1663, 5, Monster.Wood, EgyptianGods2),
    Egg(1665, 5, Monster.Light, EgyptianGods2),
    Egg(1667, 5, Monster.Dark, EgyptianGods2),

    Egg(555, 5, Monster.Fire, Ninjas),
    Egg(557, 5, Monster.Water, Ninjas),
    Egg(559, 5, Monster.Wood, Ninjas),
    Egg(561, 5, Monster.Light, Ninjas),
    Egg(563, 5, Monster.Dark, Ninjas),

    Egg(567, 5, Monster.Fire, GreekGods),
    Egg(569, 5, Monster.Water, GreekGods),
    Egg(571, 5, Monster.Wood, GreekGods),
    Egg(573, 5, Monster.Light, GreekGods),
    Egg(575, 5, Monster.Dark, GreekGods),

    Egg(620, 5, Monster.Fire, Angels),
    Egg(622, 5, Monster.Water, Angels),
    Egg(624, 5, Monster.Wood, Angels),
    Egg(626, 5, Monster.Light, Angels),
    Egg(628, 5, Monster.Dark, Angels),

    Egg(630, 5, Monster.Fire, Devils),
    Egg(632, 5, Monster.Water, Devils),
    Egg(634, 5, Monster.Wood, Devils),
    Egg(636, 5, Monster.Light, Devils),
    Egg(638, 5, Monster.Dark, Devils),

    Egg(745, 5, Monster.Fire, ChineseGods),
    Egg(747, 5, Monster.Water, ChineseGods),
    Egg(749, 5, Monster.Wood, ChineseGods),
    Egg(751, 5, Monster.Light, ChineseGods),
    Egg(753, 5, Monster.Dark, ChineseGods),

    Egg(799, 5, Monster.Fire, JapaneseGods2),
    Egg(801, 5, Monster.Water, JapaneseGods2),
    Egg(803, 5, Monster.Wood, JapaneseGods2),
    Egg(805, 5, Monster.Light, JapaneseGods2),
    Egg(807, 5, Monster.Dark, JapaneseGods2),

    Egg(2093, 5, Monster.Fire, Riders2),
    Egg(2095, 5, Monster.Water, Riders2),
    Egg(2097, 5, Monster.Wood, Riders2),
    Egg(2099, 5, Monster.Light, Riders2),
    Egg(2101, 5, Monster.Dark, Riders2),

    Egg(1372, 5, Monster.Wood, Expelleds),
    Egg(1241, 5, Monster.Wood, Expelleds),
    Egg(1359, 5, Monster.Light, Expelleds),

    Egg(414, 4, Monster.Fire, Girls),
    Egg(417, 4, Monster.Water, Girls),
    Egg(420, 4, Monster.Wood, Girls),
    Egg(423, 4, Monster.Light, Girls),
    Egg(426, 4, Monster.Dark, Girls),

    Egg(1502, 4, Monster.Fire, DragonKnights),
    Egg(1504, 4, Monster.Water, DragonKnights),
    Egg(1896, 4, Monster.Wood, DragonKnights),
    Egg(1506, 4, Monster.Light, DragonKnights),
    Egg(1898, 4, Monster.Dark, DragonKnights),

    Egg(1412, 4, Monster.Fire, FairyTales),
    Egg(1414, 4, Monster.Water, FairyTales),
    Egg(1416, 4, Monster.Wood, FairyTales),
    Egg(1418, 4, Monster.Light, FairyTales),
    Egg(1420, 4, Monster.Dark, FairyTales),

    Egg(1120, 4, Monster.Fire, PDZCollabs),
    Egg(1122, 4, Monster.Water, PDZCollabs),
    Egg(1124, 4, Monster.Wood, PDZCollabs),
    Egg(1126, 4, Monster.Light, PDZCollabs),
    Egg(1128, 4, Monster.Dark, PDZCollabs),

    Egg(1349, 4, Monster.Fire, PDZCollabs),
    Egg(1351, 4, Monster.Wood, PDZCollabs),
    Egg(1353, 4, Monster.Light, PDZCollabs),

    Egg(488, 4, Monster.Water, CouponCodes),
    Egg(486, 4, Monster.Dark, CouponCodes),

    Egg(112, 4, Monster.Fire, LateBloomerDragons),
    Egg(114, 4, Monster.Water, LateBloomerDragons),
    Egg(116, 4, Monster.Wood, LateBloomerDragons),
    Egg(118, 4, Monster.Light, LateBloomerDragons),
    Egg(120, 4, Monster.Dark, LateBloomerDragons),

    Egg(352, 4, Monster.Fire, Elementals),
    Egg(354, 4, Monster.Water, Elementals),
    Egg(356, 4, Monster.Wood, Elementals),
    Egg(358, 4, Monster.Light, Elementals),
    Egg(360, 4, Monster.Dark, Elementals)
  )

  def str2GodFestTargets(str: String) : Option[MonsterKind] = {
    val idx = godFestTargets.map{_.toString}.indexOf(str)
    if(idx == -1) None else Some(godFestTargets(idx))
  }

  val godFestTargets = Seq(
    Constellations2,
    Constellations,
    Generals,
    Angels2,
    Angels,
    IndianGods2,
    ThreeKingdomGods,
    HeroGods,
    GreekRomanGods,
    JapaneseGods,
    IndianGods,
    NorthernEuropeGods,
    EgyptianGods,
    EgyptianGods2,
    GreekGods,
    Devils,
    ChineseGods,
    JapaneseGods2
  )

  sealed trait MonsterKind
  case object GodFestLimited extends MonsterKind
  case object CarnivalLimited extends MonsterKind
  case object Constellations2 extends MonsterKind
  case object Constellations extends MonsterKind
  case object Generals extends MonsterKind
  case object MachineAnimals extends MonsterKind
  case object MachineDragons extends MonsterKind
  case object DragonKnights extends MonsterKind
  case object FairyTales extends MonsterKind
  case object Weapons extends MonsterKind
  case object Angels2 extends MonsterKind
  case object Angels extends MonsterKind
  case object MagicBooks extends MonsterKind
  case object Girls extends MonsterKind
  case object ArmedKnights extends MonsterKind
  case object Magicians extends MonsterKind
  case object IndianGods2 extends MonsterKind
  case object PDZCollabs extends MonsterKind
  case object ThreeKingdomGods extends MonsterKind
  case object HeroGods extends MonsterKind
  case object LateBloomerDragons extends MonsterKind
  case object FruitDragons extends MonsterKind
  case object GreekRomanGods extends MonsterKind
  case object JapaneseGods extends MonsterKind
  case object IndianGods extends MonsterKind
  case object Elementals extends MonsterKind
  case object NorthernEuropeGods extends MonsterKind
  case object Riders extends MonsterKind
  case object EgyptianGods extends MonsterKind
  case object EgyptianGods2 extends MonsterKind
  case object Ninjas extends MonsterKind
  case object GreekGods extends MonsterKind
  case object Devils extends MonsterKind
  case object ChineseGods extends MonsterKind
  case object JapaneseGods2 extends MonsterKind
  case object Riders2 extends MonsterKind
  case object Expelleds extends MonsterKind
  case object Valkyries extends MonsterKind
  case object CouponCodes extends MonsterKind
}
