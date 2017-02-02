package Rational

/**
  * Created by А.Скрипкин on 30.01.2017.
  */
object RichInt {

  implicit class IntFunc(a :Int) {

    @scala.annotation.tailrec
    final def gcd(b: Int) :Int =  if (0 == b) a.abs else b gcd a % b

    final def lcm(b :Int) :Int = (a*b).abs / (a gcd b)

    final def +(r :Ratio) :Ratio = Ratio(a,1) + r
    final def -(r :Ratio) :Ratio = Ratio(a,1) - r
    final def *(r :Ratio) :Ratio = Ratio(a,1) * r
    final def /(r :Ratio) :Ratio = Ratio(a,1) / r

    final def isPrime :Boolean = a.abs == 2 || !(2 to Math.sqrt(a.abs).toInt + 1).exists(a % _ == 0)

    final def pval(p :Int) :Int = {
      @scala.annotation.tailrec
      def loop(cnt :Int, rst :Int) :Int = if(rst % p > 0) cnt else loop(cnt+1, rst/p)
      require(p.isPrime, "Pvaluation possible only for prime.")
      loop(0,a.abs)
    }

    final def pow[T](n :Int) :Int = {
      def loop(a :Int, n: Int, acc: Int = 1) :Int = n match {
        case 0 => acc
        case _ if n % 2 == 0 => loop(a * a, n / 2, acc)
        case _ => loop(a,n-1,acc*a)
      }
      loop(a, n)
    }

    final def pabs(p :Int)(implicit num: Numeric[Ratio]) :Ratio = if(0==a) num.fromInt(0) else num.fromInt(p pow pval(p)).invert
  }

}
