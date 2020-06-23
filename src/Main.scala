/**
  * Created by А.Скрипкин on 30.01.2017.
  */
import Rational.Ratio._
import Rational._

object Main extends App {

    val r1 = Ratio(1,2)
    val r2 = Ratio(1,3)

    println( r1/r2 - r1*r2 )

    val r3 = Ratio(17,37)
    println (r3.toChain)
    println (fromChain(List(0, 2, 5, 1, 2)))

    println (Ratio(49,11) pabs 7)
}
