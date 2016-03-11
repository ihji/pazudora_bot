package parser

import data.{Monster, Enermy}
import fastparse.WhitespaceApi
import fastparse.noApi._

/**
  * Created by ihji on 3/7/16.
  */
object EnermyParser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import White._

  val depInfo : Parser[Enermy => Enermy] = P("방어"~num).map{case n => _.copy(dep = n)}

  val tyInfo : Parser[Enermy => Enermy] = P("타입"~ty.rep(sep=",",min=1)).map{case n => _.copy(tys = n)}

  val elemInfo : Parser[Enermy => Enermy] = P("속성"~elem).map{case n => _.copy(elem = n)}

  val enermy : Parser[Seq[Enermy=>Enermy]] = P(depInfo | tyInfo | elemInfo).rep(min=1)

  val num : Parser[Int] = P(CharIn('0' to '9').repX(1).!).map{_.toInt}

  val elem : Parser[Monster.Element] =
    P("불").map{_ => Monster.Fire} |
    P("물").map{_ => Monster.Water} |
    P("나무").map{_ => Monster.Wood} |
    P("빛").map{_ => Monster.Light} |
    P("어둠").map{_ => Monster.Dark}
  val ty : Parser[Monster.Type] =
    P("악마").map{_ => Monster.Devil} |
    P("신").map{_ => Monster.God} |
    P("공격").map{_ => Monster.Attacker} |
    P("체력 ").map{_ => Monster.Physical} |
    P("회복").map{_ => Monster.Healer} |
    P("밸런스").map{_ => Monster.Balance} |
    P("드래곤").map{_ => Monster.Dragon} |
    P("머신").map{_ => Monster.Machine} |
    P("강화합성용").map{_ => Monster.PowerUp} |
    P("진화소재").map{_ => Monster.Evolution} |
    P("능력각성").map{_ => Monster.Awake} |
    P("매각용").map{_ => Monster.Point}

  def parse(str: String) : Either[String,Enermy] = {
    enermy.parse(str) match {
      case Result.Success(e,_) =>
        val result = e.foldLeft(Enermy(0,Monster.Fire,Seq())){case (x,y) => y(x)}
        Right(result)
      case _ : Result.Failure =>
        Left("잘못된 문법입니다: "+str)
    }
  }
}
