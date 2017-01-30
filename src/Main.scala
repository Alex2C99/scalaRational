/**
  * Created by А.Скрипкин on 30.01.2017.
  */
import Rational._

object Main extends App {

  import Rational.RichInt._

  val r = 5 + Ratio(7, 12)

  println (r.toString)

}
