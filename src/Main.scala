/**
  * Created by А.Скрипкин on 30.01.2017.
  */
import Rational._

object Main extends App {

  import Rational.RichInt._

  println (Ratio(11,31).toChain)
  println (Ratio fromChain List(0, 2, 1, 4, 2))

}
