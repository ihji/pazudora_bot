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
    )
  )

  val collabProbData = Map(
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
    )
  )

  def getMonsterGroup(name: String) : Seq[Int] = {
    monsterGroups.find{_._1.toString == name}.map{_._2}.toSeq.flatten
  }

  def getCollabProb(name: String) : Map[Double, Seq[Int]] = {
    collabProbData.find{_._1.toString == name}.map{_._2}.getOrElse(Map())
  }

  sealed trait CollabGroup
  case object JuneBride extends CollabGroup
  case object YokaiWatch extends CollabGroup

  sealed trait MonsterGroup
  case object GodFestLimited extends MonsterGroup
  case object Ney extends MonsterGroup
  case object Cotten extends MonsterGroup
}
