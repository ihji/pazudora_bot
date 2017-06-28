package parser

import data.Input
import data.Input.DropSet
import fastparse.WhitespaceApi
import fastparse.noApi._

/**
  * Created by ihji on 3/7/16.
  */
object UserInputParser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import White._

  val setp : Parser[Command] = P(color ~ num ~ &("셋") ~ "셋").map{ case (c,n) => DropCommand((0 until n).map{_ => DropSet(c,3)}) }
  val twop : Parser[Command] = P(color ~ num ~ &("투웨이") ~ "투웨이").map{ case (c,n) => DropCommand((0 until n).map{_ => DropSet(c,4)}) }
  val rowp : Parser[Command] = P(color ~ num ~ &("횡") ~ "횡").map{ case (c,n) => DropCommand((0 until n).map{_ => DropSet(c,6,isRow=true)}) }
  val manyrowp : Parser[Command] = P(color ~ num ~ &("개횡") ~ "개횡").map{ case (c,n) => DropCommand(Seq(DropSet(c,n,isRow=true))) }
  val manyp : Parser[Command] = P(color ~ num ~ &("개") ~ "개").map{ case (c,n) => DropCommand(Seq(DropSet(c,n))) }
  val enhp : Parser[Command] = P("각"~num~"강화").map{ case n => EnhanceCommand(Some(n)) }
  val enhallp : Parser[Command] = P("전체"~"강화").map{ case _ => EnhanceCommand(None) }

  val damageInput : Parser[Seq[Command]] = (setp | twop | rowp | manyrowp | manyp | enhp | enhallp).rep(min = 1)

  val num : Parser[Int] = P(CharIn('0' to '9').repX(1).!).map{_.toInt}

  val color : Parser[Input.Drop] =
    P("불").map{_ => Input.Fire} |
    P("물").map{_ => Input.Water} |
    P("나무").map{_ => Input.Wood} |
    P("빛").map{_ => Input.Light} |
    P("어둠").map{_ => Input.Dark} |
    P("회복").map{_ => Input.Heart} |
    P("방해").map{_ => Input.Jammer}
  def parse(str: String) : Either[String,Input] = {
    damageInput.parse(str) match {
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
