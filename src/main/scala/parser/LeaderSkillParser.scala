package parser

import data.Monster
import Monster.Type
import fastparse.WhitespaceApi
import fastparse.noApi._

/**
  * Created by ihji on 3/7/16.
  */
object LeaderSkillParser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import White._

  val attackUp : Parser[(List[Type],Double)] =
    P("최대"~num~"배").map{ case n => (List(),n) }
  val num : Parser[Double] = P(CharIn('0' to '9', ".").repX(1).!).map{java.lang.Double.parseDouble}
}
