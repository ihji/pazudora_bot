package parser

import data.Input
import data.Input.DropSet

/**
  * Created by ihji on 3/7/16.
  */
object UserInputParser {
  import fastparse._, NoWhitespace._

  def setp[_ : P] : P[Command] = P(color ~ num ~ &("셋") ~ "셋").map{ case (c,n) => DropCommand((0 until n).map{_ => DropSet(c,3)}) }
  def twop[_ : P] : P[Command] = P(color ~ num ~ &("투웨이") ~ "투웨이").map{ case (c,n) => DropCommand((0 until n).map{_ => DropSet(c,4)}) }
  def rowp[_ : P] : P[Command] = P(color ~ num ~ &("횡") ~ "횡").map{ case (c,n) => DropCommand((0 until n).map{_ => DropSet(c,6,isRow=true)}) }
  def manyrowp[_ : P] : P[Command] = P(color ~ num ~ &("개횡") ~ "개횡").map{ case (c,n) => DropCommand(Seq(DropSet(c,n,isRow=true))) }
  def manyp[_ : P] : P[Command] = P(color ~ num ~ &("개") ~ "개").map{ case (c,n) => DropCommand(Seq(DropSet(c,n))) }
  def enhp[_ : P] : P[Command] = P("각"~num~"강화").map{ case n => EnhanceCommand(Some(n)) }
  def enhallp[_ : P] : P[Command] = P("전체"~"강화").map{ case _ => EnhanceCommand(None) }

  def damageInput[_ : P] : P[Seq[Command]] = (setp | twop | rowp | manyrowp | manyp | enhp | enhallp).rep(1)

  def num[_ : P] : P[Int] = P(CharIn("0-9").repX(1).!).map{_.toInt}

  def color[_ : P] : P[Input.Drop] =
    P("불").map{_ => Input.Fire} |
    P("물").map{_ => Input.Water} |
    P("나무").map{_ => Input.Wood} |
    P("빛").map{_ => Input.Light} |
    P("어둠").map{_ => Input.Dark} |
    P("회복").map{_ => Input.Heart} |
    P("방해").map{_ => Input.Jammer}
  def parseInput(str: String) : Either[String,Input] = {
    parse(str, damageInput(_)) match {
      case Parsed.Success(seq,_) =>
        val (result,remaining) = seq.foldLeft(Input.empty,Seq.empty[DropSet]) {
          case ((i,d),c) =>
            c match {
              case DropCommand(drops) => (i,d ++ drops)
              case EnhanceCommand(opt) =>
                if(opt.nonEmpty) (d.map{x => x.copy(numEnhanced = x.num min opt.get)}.foldLeft(i){_.add(_)},Seq())
                else (d.map{x => x.copy(numEnhanced = x.num)}.foldLeft(i){_.add(_)},Seq())
            }
        }
        Right(remaining.foldLeft(result){_.add(_)})
      case _ : Parsed.Failure =>
        Left("잘못된 문법입니다: "+str)
    }
  }

  sealed trait Command
  case class DropCommand(drops: Seq[DropSet]) extends Command
  case class EnhanceCommand(enh: Option[Int]) extends Command
}
