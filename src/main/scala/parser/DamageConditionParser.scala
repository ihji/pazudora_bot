package parser

import data.Input
import fastparse.WhitespaceApi
import fastparse.noApi._

/**
  * Created by ihji on 3/7/16.
  */
object DamageConditionParser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import White._

  val setp : Parser[Input => Input] = P(num ~ &("셋") ~ "셋").map{ case n => _.copy(set = n.toInt) }
  val twop : Parser[Input => Input] = P(num ~ &("투웨이") ~ "투웨이").map{ case n => _.copy(twoway = n.toInt) }
  val combop : Parser[Input => Input] = P(num ~ &("콤") ~ "콤").map{ case n => _.copy(combo = n.toInt) }
  val rowp : Parser[Input => Input] = P(num ~ &("횡") ~ "횡").map{ case n => _.copy(row = n.toInt) }

  val damageInput : Parser[Seq[Input => Input]] = (setp | twop | combop | rowp).rep

  val num : Parser[Double] = P(CharIn('0' to '9', ".").repX(1).!).map{java.lang.Double.parseDouble}

  def parse(str: String) : Either[String,Input] = {
    damageInput.parse(str) match {
      case Result.Success(seq,_) =>
        Right(seq.foldRight(Input.empty){_(_)})
      case _ : Result.Failure =>
        Left("잘못된 문법입니다: "+str)
    }
  }
}
