/**
  * Created by А.Скрипкин on 30.01.2017.
  */
import Rational.Ratio._
import Rational._

object Main extends App {

  println (Ratio(11,31).toChain)
  println (fromChain(List(0, 2, 1, 4, 2)))

  println (Ratio(49*49,49*11) pabs 7)
}
