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

  val setp : Parser[Input => Input] = P(color ~ num ~ &("셋") ~ "셋").map{ case (c,n) => x => (0 until n).foldLeft(x){case (i,_) => i.add(DropSet(c,3))} }
  val twop : Parser[Input => Input] = P(color ~ num ~ &("투웨이") ~ "투웨이").map{ case (c,n) => x => (0 until n).foldLeft(x){case (i,_) => i.add(DropSet(c,4))} }
  val rowp : Parser[Input => Input] = P(color ~ num ~ &("횡") ~ "횡").map{ case (c,n) => x => (0 until n).foldLeft(x){case (i,_) => i.add(DropSet(c,6,isRow=true))} }

  val damageInput : Parser[Seq[Input => Input]] = (setp | twop | rowp).rep(min = 1)

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
      case Result.Success(seq,_) =>
        Right(seq.foldRight(Input.empty){_(_)})
      case _ : Result.Failure =>
        Left("잘못된 문법입니다: "+str)
    }
  }
}
