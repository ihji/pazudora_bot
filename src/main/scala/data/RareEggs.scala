package data

import java.util.Date

import scala.util.Random

/**
  * Created by heejong.lee on 3/22/16.
  */
case class RareEggs(prob: Map[Double, Seq[Int]]) {
  val random = new Random(new Date().getTime())

  lazy val targets =
    prob.toList.flatMap{
      case (p, s) =>
        (1L to Math.round(p * 10)).flatMap{_ => s}
    }

  def pick() : Int = {
    val idx = random.nextInt(targets.length)
    targets(idx)
  }
}

object RareEggs {
  case class Egg(id: Int, rarity: Int, elem: Monster.Element, kind: MonsterGroup)

  val monsterGroups = Map(
    GodFestLimited -> Seq(
      365,  // 청딘
      643, // 흑타
      914, // 녹소
      1089, // 청소
      1108, // 적딘
      1588, // 암칼리
      1674, // 스쿨드
      1949, // 티폰
      1951, // 이르무
      2146, // 카에데
      2148, // 칸나
      2443, // 류네
      2509, // 실비
      2565, // 쉐아트
      2592, // 에스카마리
      2713, // 발딘
      2900, // 펜니르 비즈
      2943, // 셰리아스 루츠
      2991, // 단탈리온
      2997, // 파이몬
      2999, // 그레모리
      3268, // 아텐
      3414, // 풍신
      3416, // 뇌신
      3603, // 아메노미나카누시
      3646, // 요그소토스
      3757, // 불네이
      3758, // 물네이
      3759, // 나무네이
      3760, // 빛네이
      3761, // 어둠네이
      3786, // 카미무스비
      3897, // 불코튼
      3898, // 물코튼
      3899, // 나무코튼
      3900, // 빛코튼
      3901, // 어둠코튼
      4058, // 마하
      4062, // 모리그
      4410, // 마두
      4412, // 리체
      4414, // 제라
      4647, // 사레네
      4649, // 벨로아
      4834, // 발키리 시엘
      4836, // 제우스 기가
      4838, // 아테나 논
      5125, // 구참공
      5127, // 가란도우지
      5129, // 용기사 렉스
      5131, // 에키드나 사라
      5133, // 헤라 루나
      5234  // 쉘링포드
    ),
    Ney -> Seq(
      3757, // 불네이
      3758, // 물네이
      3759, // 나무네이
      3760, // 빛네이
      3761  // 어둠네이
    ),
    Cotten -> Seq(
      3897, // 불코튼
      3898, // 물코튼
      3899, // 나무코튼
      3900, // 빛코튼
      3901  // 어둠코튼
    ),
    Witch -> Seq(
      4410, // 마두
      4412, // 리체
      4414, // 제라
      4647, // 사레네
      4649  // 벨로아
    ),
    Japanese1 -> Seq(
      1726, // 각성 카구츠치
      2322, // 각성 오로치
      2323, // 각성 스사노오노
      2324, // 각성 아마테라스
      2325  // 각성 츠쿠요미
    ),
    Indian1 -> Seq(
      1954, // 각성 시바
      1955, // 각성 락슈미
      1956, // 각성 파르바티
      2979, // 각성 인드라
      2980  // 각성 브리트라
    ),
    Egypt1 -> Seq(
      2009, // 각성 호루스
      2010, // 각성 이시스
      2011, // 각성 바스테트
      2012, // 각성 라
      2013  // 각성 아누비스
    ),
    Sabang -> Seq(
      2073, // 각성 레이란
      2074, // 각성 카린
      2075, // 각성 메이메이
      2389, // 각성 사쿠야
      2076  // 각성 하쿠
    ),
    Hero -> Seq(
      3509, // 각성 야마토
      3510, // 각성 안드로메다
      3511, // 각성 페르세우스
      3512, // 각성 손오공
      2662  // 각성 판도라
    ),
    Indian2 -> Seq(
      3592, // 각성 크리슈나
      3593, // 각성 사라스바티
      3594, // 각성 비슈누
      3595, // 각성 가네샤
      3596  // 각성 두르가
    ),
    Jeongook -> Seq(
      3840, // 각성 사나다
      3841, // 각성 모리
      3842, // 각성 이시다
      3843, // 각성 마에다
      3844  // 각성 아케치
    ),
    Myoungwang -> Seq(
      4248, // 각성 항삼세명왕
      4250, // 각성 군다리명왕
      4252, // 각성 부동명왕
      4254, // 각성 금강야차명왕
      4256  // 각성 대위덕명왕
    )
  )

  val probData = Map(
    JuneBride -> Map(
      1.0 -> Seq(5382),
      1.5 -> Seq(4588, 4589, 2949, 3790),
      2.0 -> Seq(5381, 2951, 5380, 3791),
      7.5 -> Seq(2953, 2952),
      13.6 -> Seq(2955, 2956, 2954),
      14.6 -> Seq(2957, 2958)
    ),
    YokaiWatch -> Map(
      2.5 -> Seq(4949, 4955, 4952, 4957, 4959),
      3.5 -> Seq(4965),
      14.0 -> Seq(4961, 4971, 4963, 4967, 4973, 4969)
    ),
    FinalFantasy -> Map(
      2.5 -> Seq(3796, 2029, 2043, 2778, 3297, 5394, 2031, 2767, 3304),
      3.5 -> Seq(2033, 2772, 3295, 3798, 2035, 2041, 2781, 3800, 2037, 2770, 2776, 2774),
      3.0 -> Seq(5396),
      4.06 -> Seq(2045, 2783, 3300, 2047, 3302, 2049, 3802, 2039)
    ),
    GF190630 -> Map(
      0.3 -> (monsterGroups(Ney) ++ monsterGroups(Cotten)),
      1.5 -> (monsterGroups(Witch) ++
              Seq(5133, 4834, 4838, 5125, 5127, 5234, 643, 1588, 1949, 1951, 2146, 2997, 3268, 3786, 4062)),
      1.68 -> (monsterGroups(Japanese1) ++ monsterGroups(Indian1) ++ monsterGroups(Indian2) ++ monsterGroups(Egypt1) ++
               monsterGroups(Sabang) ++ monsterGroups(Hero) ++ monsterGroups(Jeongook) ++ monsterGroups(Myoungwang))
    )
  )

  def getMonsterGroup(name: String) : Seq[Int] = {
    monsterGroups.find{_._1.toString == name}.map{_._2}.toSeq.flatten
  }

  def getProb(name: String) : Map[Double, Seq[Int]] = {
    probData.find{_._1.toString == name}.map{_._2}.getOrElse(Map())
  }

  sealed trait ProbGroup
  case object JuneBride extends ProbGroup
  case object YokaiWatch extends ProbGroup
  case object FinalFantasy extends ProbGroup
  case object GF190630 extends ProbGroup

  sealed trait MonsterGroup
  case object GodFestLimited extends MonsterGroup
  case object Ney extends MonsterGroup
  case object Cotten extends MonsterGroup
  case object Witch extends MonsterGroup
  case object Japanese1 extends MonsterGroup
  case object Indian1 extends MonsterGroup
  case object Indian2 extends MonsterGroup
  case object Egypt1 extends MonsterGroup
  case object Sabang extends MonsterGroup
  case object Hero extends MonsterGroup
  case object Jeongook extends MonsterGroup
  case object Myoungwang extends MonsterGroup
}
